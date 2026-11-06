%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2026 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module encapsulates high-level logic related to user accounts.
%%% @end
%%%-------------------------------------------------------------------
-module(user_account).
-author("Lukasz Opiola").

-include("auth/entitlement_mapping.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/logging.hrl").


%% API
-export([gen_user_id/1, gen_user_id/2]).
-export([create/4]).
-export([update_attributes/3]).
-export([acquire_user/2, link_account/2]).
-export([build_test_user_info/1]).


% Indicates what process/logic/flow has caused the user account to be created
-type creation_context() :: onepanel_account_migration | user_creation_api | idp_auth:flow_type().


%%%===================================================================
%%% API functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% @equiv gen_user_id(IdP, SubjectId)
%% @end
%%--------------------------------------------------------------------
-spec gen_user_id(linked_account:t()) -> od_user:id().
gen_user_id(#linked_account{idp = IdP, subject_id = SubjectId}) ->
    gen_user_id(IdP, SubjectId).


%%--------------------------------------------------------------------
%% @doc
%% Constructs user id based on IdP name and user's subjectId in that IdP.
%% Onezone versions pre 19.02.1 used legacy key mapping - checks if such user
%% is present and if so, reuses the legacy id to retain the user mapping after
%% upgrade. Otherwise, returns an id constructed using the new procedure.
%% @end
%%--------------------------------------------------------------------
-spec gen_user_id(auth_config:idp(), SubjectId :: binary()) -> od_user:id().
gen_user_id(IdP, SubjectId) ->
    LegacyUserId = datastore_key:gen_legacy_key(<<"">>, str_utils:format_bin("~ts:~ts", [IdP, SubjectId])),
    case user_logic:exists(LegacyUserId) of
        true -> LegacyUserId;
        false -> linked_account:digest_based_id(IdP, SubjectId)
    end.


-spec create(od_user:id() | undefined, od_user:record(), [linked_account:t()], creation_context()) ->
    {ok, od_user:doc()} | errors:error().
create(undefined, InitialUserRecord, [], CreationContext) ->
    create(datastore_key:new(), InitialUserRecord, [], CreationContext);

create(undefined, InitialUserRecord, [First | _] = LinkedAccounts, CreationContext) ->
    MappedUserId = gen_user_id(First),
    create(MappedUserId, InitialUserRecord, LinkedAccounts, CreationContext);

create(ProposedUserId, InitialUserRecord, LinkedAccounts, CreationContext) ->
    Result = ?catch_exceptions(od_user:critical_section(ProposedUserId, fun() ->
        ?check(od_user:exists(ProposedUserId)) andalso throw(?ERROR_ALREADY_EXISTS),

        Username = resolve_username_for_new_account(InitialUserRecord, LinkedAccounts),

        {ok, od_user:critical_section(Username, fun() ->
            create_unsafe(
                ProposedUserId,
                InitialUserRecord#od_user{
                    username = Username,
                    full_name = resolve_full_name_for_new_account(InitialUserRecord, LinkedAccounts)
                },
                LinkedAccounts,
                CreationContext
            )
        end)}
    end)),

    case Result of
        {ok, UserDoc} ->
            set_up_new_account(ProposedUserId),
            entity_graph:ensure_up_to_date(),
            {ok, UserDoc};
        {error, _} = Error ->
            Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Providing 'undefined' values means that the attribute should not be changed.
%%--------------------------------------------------------------------
-spec update_attributes(od_user:id(), undefined | od_user:username(), undefined | od_user:full_name()) ->
    ok | {error, term()}.
update_attributes(UserId, NewUsername, NewFullName) ->
    od_user:critical_section(NewUsername, fun() ->
        case od_user:get_by_username(NewUsername) of
            {ok, #document{key = OtherUserId}} when OtherUserId /= UserId ->
                % the username is occupied by another user
                ?ERR_BAD_VALUE_IDENTIFIER_OCCUPIED(?err_ctx(), <<"username">>);
            _ ->
                % no other user occupies the username or it's already held by this user
                od_user:update_unsafe(UserId, fun(UserRecord) ->
                    {ok, UserRecord#od_user{
                        username = utils:ensure_defined(NewUsername, UserRecord#od_user.username),
                        full_name = utils:ensure_defined(NewFullName, UserRecord#od_user.full_name)
                    }}
                end),
                ok
        end
    end).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a user by given linked account and merges the carried information.
%% If such user does not exist, creates a new user based on that linked account.
%% Checks if the user is blocked and returns an error if so.
%% @end
%%--------------------------------------------------------------------
-spec acquire_user(linked_account:t(), idp_auth:flow_type()) ->
    {ok, od_user:doc()} | errors:error().
acquire_user(LinkedAccount, FlowType) ->
    case od_user:get_by_linked_account(LinkedAccount) of
        {ok, #document{value = #od_user{blocked = true}}} ->
            ?ERR_USER_BLOCKED(?err_ctx());
        {ok, #document{key = UserId}} ->
            merge_linked_account(UserId, LinkedAccount);
        ?ERROR_NOT_FOUND ->
            create(gen_user_id(LinkedAccount), #od_user{}, [LinkedAccount], FlowType)
    end.


-spec link_account(od_user:id(), linked_account:t()) -> ok | {error, term()}.
link_account(TargetUserId, LinkedAccount) ->
    % Check if this account isn't connected to other profile
    case od_user:get_by_linked_account(LinkedAccount) of
        {ok, #document{key = FoundUserId}} ->
            % Synchronize the information regardless of account linking success
            merge_linked_account(FoundUserId, LinkedAccount),
            case FoundUserId of
                TargetUserId ->
                    {error, already_linked_to_itself};
                OtherUserId ->
                    {error, {already_linked_to_other, OtherUserId}}
            end;

        ?ERROR_NOT_FOUND ->
            % ok, add new linked account to the user
            ?extract_ok(merge_linked_account(TargetUserId, LinkedAccount))
    end.


%%--------------------------------------------------------------------
%% @doc
%% Build a JSON compatible user info based on a linked account for test page
%% purposes. The info expresses what user data would be gathered during an
%% analogous production login process.
%% @end
%%--------------------------------------------------------------------
-spec build_test_user_info(linked_account:t()) ->
    {od_user:id(), json_utils:json_term()}.
build_test_user_info(LinkedAccount) ->
    #linked_account{
        idp = IdP,
        full_name = FullName,
        username = Username,
        emails = Emails,
        entitlements = Entitlements
    } = LinkedAccount,
    MappedEntitlements = entitlement_mapping:map_entitlements(IdP, Entitlements),
    UserId = gen_user_id(LinkedAccount),
    {UserId, #{
        <<"userId">> => UserId,
        <<"fullName">> => entity_logic_sanitizer:normalize_full_name(FullName),
        <<"username">> => entity_logic_sanitizer:normalize_username(Username),
        <<"emails">> => linked_account:normalize_emails(Emails),
        <<"linkedAccounts">> => [linked_account:to_json(LinkedAccount)],
        <<"groups">> => maps:from_list(
            lists:map(fun({GroupId, #idp_entitlement{privileges = Privileges}}) ->
                {GroupId, Privileges}
            end, MappedEntitlements)
        )
    }}.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc
%% NOTE: MUST be called within @see od_user:critical_section/2.
%% Checks for Id / username collisions must be made on the higher level.
%% @end
%%--------------------------------------------------------------------
-spec create_unsafe(
    od_user:id() | undefined,
    od_user:record(),
    [linked_account:t()],
    creation_context()
) ->
    od_user:doc().
create_unsafe(UserId, UserRecord, LinkedAccounts, CreationContext) ->
    UserDoc = od_user:create_unsafe(#document{
        key = UserId,
        value = UserRecord#od_user{
            creation_time = global_clock:timestamp_seconds()
        }
    }),

    {IdP, SubjectId} = case LinkedAccounts of
        [] ->
            {<<"none">>, <<"none">>};
        [#linked_account{idp = LAIdP, subject_id = LASubject} | _] ->
            {LAIdP, LASubject}
    end,

    ?notice(
        "New user account has been created:~n"
        "> reason:    ~ts~n"
        "> userId:    ~ts~n"
        "> fullName:  ~ts~n"
        "> username:  ~ts~n"
        "> IdP:       ~ts~n"
        "> subjectId: ~ts", [
            case CreationContext of
                onepanel_account_migration -> "Onepanel account migration";
                user_creation_api -> "invocation via the API";
                gui_login -> "first login via GUI";
                access_token -> "first authentication using a delegated IdP access token"
            end,
            UserId,
            UserRecord#od_user.full_name,
            UserRecord#od_user.username,
            IdP,
            SubjectId
        ]
    ),

    lists:foldl(fun(LinkedAccount, AccUserDoc) ->
        merge_linked_account_unsafe(AccUserDoc, LinkedAccount)
    end, UserDoc, LinkedAccounts).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% NOTE: MUST be called within @see od_user:critical_section/2.
%% Blocks until user's effective relations have been fully synchronized.
%% @end
%%--------------------------------------------------------------------
-spec merge_linked_account(od_user:id(), linked_account:t()) -> {ok, od_user:doc()} | errors:error().
merge_linked_account(UserId, LinkedAccount) ->
    Result = od_user:critical_section(UserId, fun() ->
        ?catch_exceptions({ok, merge_linked_account_unsafe(UserId, LinkedAccount)})
    end),
    case Result of
        {ok, Doc} ->
            entity_graph:ensure_up_to_date(),
            {ok, Doc};
        {error, _} = Error ->
            Error
    end.


%% @private
%% @doc NOTE: MUST be called within @see od_user:critical_section/2
-spec merge_linked_account_unsafe(od_user:id() | od_user:doc(), linked_account:t()) -> od_user:doc().
merge_linked_account_unsafe(UserId, LinkedAccount) when is_binary(UserId) ->
    merge_linked_account_unsafe(?check(od_user:get(UserId)), LinkedAccount);

merge_linked_account_unsafe(#document{key = UserId, value = #od_user{
    entitlements = PreviousEntitlements
}}, LinkedAccount) ->
    UpdatedUserDoc = ?check(od_user:update_unsafe(UserId, fun(UserRecord) ->
        {ok, linked_account:merge_into_user_record(
            LinkedAccount, UserId, UserRecord
        )}
    end)),

    % this causes adding/removing the user from groups and cannot be called from the tp process
    NewEntitlements = entitlement_mapping:coalesce_entitlements(
        UserId, UpdatedUserDoc#document.value#od_user.linked_accounts, PreviousEntitlements
    ),

    % the update is two-phase, as we do not want to update the information about
    % entitlements before they are successfully coalesced - otherwise, a failure of
    % this process would leave the information in the doc desynchronized with the
    % actual group memberships
    ?check(od_user:update_unsafe(UserId, fun(UserRecord) ->
        {ok, UserRecord#od_user{
            entitlements = NewEntitlements
        }}
    end)).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% full name precedence (?DEFAULT_FULL_NAME is considered undefined):
%% 1. The value provided in the initial user record, if defined
%% 2. The first valid full name from the linked accounts (after normalization)
%% 3. ?DEFAULT_FULL_NAME, if none of the above
%% @end
%%--------------------------------------------------------------------
-spec resolve_username_for_new_account(od_user:record(), [linked_account:t()]) -> undefined | od_user:username().
resolve_username_for_new_account(#od_user{username = undefined}, LinkedAccounts) ->
    % if no specific username was requested, it may be taken from one of the linked accounts,
    % but best-effort - if they occupied, the creation should not end with an error
    lists_utils:foldl_while(fun(#linked_account{username = LAUsername}, Acc) ->
        case entity_logic_sanitizer:normalize_username(LAUsername) of
            undefined ->
                {cont, Acc};
            NormalizedUsername ->
                case od_user:get_by_username(NormalizedUsername) of
                    {ok, _} -> {cont, Acc};
                    ?ERROR_NOT_FOUND -> {halt, NormalizedUsername}
                end
        end
    end, undefined, LinkedAccounts);

resolve_username_for_new_account(#od_user{username = ProposedUsername}, _LinkedAccounts) ->
    % if the username was explicitly requested and it's occupied, user creation must be aborted
    case od_user:get_by_username(ProposedUsername) of
        {ok, _} ->
            throw(?ERR_BAD_VALUE_IDENTIFIER_OCCUPIED(?err_ctx(), <<"username">>));
        ?ERROR_NOT_FOUND ->
            ProposedUsername
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% username precedence:
%% 1. The value provided in the initial user record, if defined
%% 2. The first valid username from the linked accounts (after normalization)
%% 3. undefined, if none of the above
%% @end
%%--------------------------------------------------------------------
-spec resolve_full_name_for_new_account(od_user:record(), [linked_account:t()]) -> undefined | od_user:username().
resolve_full_name_for_new_account(#od_user{full_name = ?DEFAULT_FULL_NAME}, LinkedAccounts) ->
    % if no specific full name was requested, it may be taken from one of the linked accounts,
    % but must conform to the naming rules
    lists_utils:foldl_while(fun(#linked_account{full_name = FullName}, Acc) ->
        case entity_logic_sanitizer:normalize_full_name(FullName) of
            ?DEFAULT_FULL_NAME -> {cont, Acc};
            NormalizedFullName -> {halt, NormalizedFullName}
        end
    end, ?DEFAULT_FULL_NAME, LinkedAccounts);

resolve_full_name_for_new_account(#od_user{full_name = ProposedFullName}, _LinkedAccounts) ->
    ProposedFullName.


%% @private
-spec set_up_new_account(od_user:id()) -> ok.
set_up_new_account(UserId) ->
    case oz_worker:get_env(enable_automatic_first_space, false) of
        true ->
            {ok, _} = user_logic:create_space(?USER(UserId), UserId, ?FIRST_SPACE_NAME);
        _ ->
            ok
    end,

    case oz_worker:get_env(enable_global_groups, false) of
        true ->
            GlobalGroups = oz_worker:get_env(global_groups),
            lists:foreach(fun({GroupId, Privileges}) ->
                {ok, UserId} = group_logic:add_user(?ROOT, GroupId, UserId, Privileges),
                ?info("User '~ts' has been added to global group '~ts'", [UserId, GroupId])
            end, GlobalGroups);
        _ ->
            ok
    end.
