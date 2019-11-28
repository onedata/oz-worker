%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module encapsulates logic related to users' linked accounts.
%%% @end
%%%-------------------------------------------------------------------
-module(linked_accounts).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").

%% API
-export([to_map/2, to_maps/2]).
-export([gen_user_id/1, gen_user_id/2]).
-export([find_user/1, acquire_user/1]).
-export([merge/2]).
-export([build_test_user_info/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Converts a linked account into a serializable map. Scope can be one of:
%%  * all_fields - returns all fields of the linked account; intended for
%%                 the owner user as it includes private data
%%  * luma_payload - returns the fields used for user mapping in LUMA, stripped
%%                   of some unnecessary / private data.
%% @end
%%--------------------------------------------------------------------
-spec to_map(od_user:linked_account(), Scope :: all_fields | luma_payload) -> map().
to_map(LinkedAccount, Scope) ->
    #linked_account{
        idp = IdP,
        subject_id = SubjectId,
        full_name = FullName,
        username = Username,
        emails = Emails,
        entitlements = Entitlements,
        custom = Custom
    } = LinkedAccount,

    %% @TODO VFS-4506 fullName and entitlements are no longer
    % sent to LUMA as they are ambiguous and inconclusive for user mapping
    {FullNameValue, EntitlementsValue} = case Scope of
        all_fields -> {FullName, Entitlements};
        luma_payload -> {undefined, []}
    end,

    #{
        <<"idp">> => IdP,
        <<"subjectId">> => SubjectId,
        <<"fullName">> => gs_protocol:undefined_to_null(FullNameValue),
        <<"username">> => gs_protocol:undefined_to_null(Username),
        <<"emails">> => Emails,
        <<"entitlements">> => EntitlementsValue,
        <<"custom">> => Custom,

        %% @TODO VFS-4506 deprecated, included for backward compatibility
        <<"name">> => gs_protocol:undefined_to_null(FullNameValue),
        <<"login">> => gs_protocol:undefined_to_null(Username),
        <<"alias">> => gs_protocol:undefined_to_null(Username),
        <<"emailList">> => Emails,
        <<"groups">> => EntitlementsValue
    }.


%%--------------------------------------------------------------------
%% @doc
%% Converts a list of linked_account records into a serializable list of maps.
%% @end
%%--------------------------------------------------------------------
-spec to_maps([od_user:linked_account()], Scope :: all_fields | luma_payload) -> [map()].
to_maps(LinkedAccounts, Scope) ->
    [to_map(L, Scope) || L <- LinkedAccounts].


%%--------------------------------------------------------------------
%% @doc
%% @equiv gen_user_id(IdP, SubjectId)
%% @end
%%--------------------------------------------------------------------
-spec gen_user_id(od_user:linked_account()) -> od_user:id().
gen_user_id(#linked_account{idp = IdP, subject_id = SubjectId}) ->
    gen_user_id(IdP, SubjectId).


%%--------------------------------------------------------------------
%% @doc
%% Constructs user id based on IdP name and user's subjectId in that IdP.
%% @end
%%--------------------------------------------------------------------
-spec gen_user_id(auth_config:idp(), SubjectId :: binary()) -> od_user:id().
gen_user_id(IdP, SubjectId) ->
    datastore_utils:gen_key(<<"">>, str_utils:format_bin("~ts:~s", [IdP, SubjectId])).


%%--------------------------------------------------------------------
%% @doc
%% Returns the user doc linked to given account or {error, not_found}.
%% @end
%%--------------------------------------------------------------------
-spec find_user(od_user:linked_account()) ->
    {ok, od_user:doc()} | {error, not_found}.
find_user(LinkedAccount) ->
    od_user:get_by_linked_account(LinkedAccount).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a user by given linked account and merges the carried information.
%% If such user does not exist, creates a new user based on that linked account.
%% @end
%%--------------------------------------------------------------------
-spec acquire_user(od_user:linked_account()) -> {ok, od_user:doc()}.
acquire_user(LinkedAccount) ->
    case find_user(LinkedAccount) of
        {ok, #document{key = UserId}} ->
            merge(UserId, LinkedAccount);
        {error, not_found} ->
            create_user(LinkedAccount)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Adds a linked account to user's account or replaces the old one (if
%% present). Gathers emails into user's account in the process. Blocks until
%% user's effective relations have been fully synchronized.
%% @end
%%--------------------------------------------------------------------
-spec merge(od_user:id(), od_user:linked_account()) -> {ok, od_user:doc()}.
merge(UserId, LinkedAccount) ->
    % The update cannot be done in one transaction, because linked account
    % merging causes adding/removing the user from groups, which modifies user
    % doc and would cause a deadlock. Instead, use a critical section to make
    % sure that merging accounts is sequential.
    {ok, Doc} = critical_section:run({merge_acc, UserId}, fun() ->
        merge_unsafe(UserId, LinkedAccount)
    end),
    entity_graph:ensure_up_to_date(),
    {ok, Doc}.


%%--------------------------------------------------------------------
%% @doc
%% Build a JSON compatible user info based on a linked account for test page
%% purposes. The info expresses what user data would be gathered during an
%% analogous production login process.
%% @end
%%--------------------------------------------------------------------
-spec build_test_user_info(od_user:linked_account()) ->
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
    {GroupIds, _} = lists:unzip(MappedEntitlements),
    UserId = gen_user_id(LinkedAccount),
    {UserId, #{
        <<"userId">> => UserId,
        <<"fullName">> => user_logic:normalize_full_name(FullName),
        <<"username">> => user_logic:normalize_username(Username),
        <<"emails">> => normalize_emails(Emails),
        <<"linkedAccounts">> => [to_map(LinkedAccount, all_fields)],
        <<"groups">> => GroupIds
    }}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates a new user based on given linked account. Before creating such user,
%% it must be ensured that a user with such linked account does not exist.
%% @end
%%--------------------------------------------------------------------
-spec create_user(od_user:linked_account()) -> {ok, od_user:doc()}.
create_user(LinkedAccount = #linked_account{full_name = FullName, username = Username}) ->
    ProposedUserId = gen_user_id(LinkedAccount),
    {ok, UserId} = user_logic:create(?ROOT, ProposedUserId, #{
        <<"fullName">> => user_logic:normalize_full_name(FullName)
    }),
    % Setting the username might fail (if it's not unique) - it's not considered a failure.
    user_logic:update_username(?ROOT, UserId, user_logic:normalize_username(Username)),
    merge(UserId, LinkedAccount).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Adds a linked account to user's account or replaces the old one (if
%% present). Gathers emails and entitlements into user's account in the process.
%% This code must not be run in parallel.
%% @end
%%--------------------------------------------------------------------
-spec merge_unsafe(od_user:id(), od_user:linked_account()) ->
    {ok, od_user:doc()}.
merge_unsafe(UserId, LinkedAccount) ->
    {ok, #document{value = #od_user{
        emails = Emails, linked_accounts = LinkedAccounts, entitlements = OldEntitlements
    } = UserInfo}} = od_user:get(UserId),
    #linked_account{
        idp = IdP, subject_id = SubjectId, emails = LinkedEmails,
        access_token = NewAccessT, refresh_token = NewRefreshT
    } = LinkedAccount,
    % Add (normalized), valid emails from the IdP that are not yet added to the account
    NewEmails = lists:usort(Emails ++ normalize_emails(LinkedEmails)),

    % Replace existing linked account, if present
    NewLinkedAccs = case find_linked_account(UserInfo, IdP, SubjectId) of
        OldLinkedAcc = #linked_account{access_token = OldAccessT, refresh_token = OldRefreshT} ->
            LinkedAccCoalescedTokens = LinkedAccount#linked_account{
                access_token = case NewAccessT of {undefined, _} -> OldAccessT; _ -> NewAccessT end,
                refresh_token = case NewRefreshT of undefined -> OldRefreshT; _ -> NewRefreshT end
            },
            lists:delete(OldLinkedAcc, LinkedAccounts) ++ [LinkedAccCoalescedTokens];
        undefined ->
            LinkedAccounts ++ [LinkedAccount]
    end,

    NewEntitlements = entitlement_mapping:coalesce_entitlements(
        UserId, NewLinkedAccs, OldEntitlements
    ),

    % Return updated user info
    od_user:update(UserId, fun(User = #od_user{}) ->
        {ok, User#od_user{
            emails = NewEmails,
            linked_accounts = NewLinkedAccs,
            entitlements = NewEntitlements
        }}
    end).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Finds a linked account in user doc based on IdP and user id in that IdP.
%% Returns undefined upon failure.
%% @end
%%--------------------------------------------------------------------
-spec find_linked_account(od_user:record(), auth_config:idp(),
    SubjectId :: binary()) -> undefined | od_user:linked_account().
find_linked_account(#od_user{linked_accounts = LinkedAccounts}, IdP, SubjectId) ->
    lists:foldl(fun(LinkedAccount, Acc) ->
        case LinkedAccount of
            #linked_account{idp = IdP, subject_id = SubjectId} ->
                LinkedAccount;
            _ ->
                Acc
        end
    end, undefined, LinkedAccounts).


%% @private
-spec normalize_emails([binary()]) -> [binary()].
normalize_emails(Emails) ->
    lists:filtermap(fun(Email) ->
        Normalized = http_utils:normalize_email(Email),
        case http_utils:validate_email(Normalized) of
            true -> {true, Normalized};
            false -> false
        end
    end, Emails).