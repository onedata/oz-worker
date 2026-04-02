%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2026 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements the #linked_account{} jsonable record and
%%% related utils.
%%% @end
%%%-------------------------------------------------------------------
-module(linked_account).
-author("Lukasz Opiola").

-behaviour(jsonable_record).

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").


-type t() :: #linked_account{}.
-export_type([t/0]).

%% API
-export([digest_based_id/1, digest_based_id/2]).
-export([find/2, find/3]).
-export([merge_into_user_record/3]).
-export([normalize_emails/1]).
-export([apply_luma_info_mask/1]).


%% Jsonable record callbacks
-export([to_json/1, from_json/1]).


%%%===================================================================
%%% API functions
%%%===================================================================


-spec digest_based_id(t()) -> binary().
digest_based_id(#linked_account{idp = IdP, subject_id = SubjectId}) ->
    digest_based_id(IdP, SubjectId).


-spec digest_based_id(auth_config:idp(), SubjectId :: binary()) -> binary().
digest_based_id(IdP, SubjectId) ->
    datastore_key:new_from_digest([atom_to_binary(IdP, utf8), SubjectId]).


-spec find([linked_account:t()], DigestBasedId :: binary()) ->
    error | {ok, linked_account:t()}.
find(LinkedAccounts, DigestBasedId) ->
    lists_utils:foldl_while(fun(LinkedAccount, Acc) ->
        case digest_based_id(LinkedAccount) of
            DigestBasedId ->
                {halt, {ok, LinkedAccount}};
            _ ->
                {cont, Acc}
        end
    end, error, LinkedAccounts).


-spec find([linked_account:t()], auth_config:idp(), binary()) -> error | {ok, t()}.
find(LinkedAccounts, IdP, SubjectId) ->
    find(LinkedAccounts, digest_based_id(IdP, SubjectId)).


-spec merge_into_user_record(t(), od_user:id(), od_user:record()) -> od_user:record().
merge_into_user_record(
    #linked_account{
        idp = IdP,
        subject_id = SubjectId,
        emails = LinkedEmails,
        access_token = NewAccessT,
        refresh_token = NewRefreshT
    } = LinkedAccount,
    UserId,
    #od_user{
        full_name = FullName,
        username = Username,
        emails = Emails,
        linked_accounts = LinkedAccounts
    } = UserRecord
) ->
    NewEmails = lists:usort(Emails ++ normalize_emails(LinkedEmails)),

    % Replace existing linked account, if present
    NewLinkedAccounts = case find(LinkedAccounts, IdP, SubjectId) of
        error ->
            ?notice(
                "A user has had a new IdP account linked:~n"
                "> userId:    ~ts~n"
                "> fullName:  ~ts~n"
                "> username:  ~ts~n"
                "> IdP:       ~ts~n"
                "> subjectId: ~ts", [
                UserId,
                FullName,
                Username,
                LinkedAccount#linked_account.idp,
                LinkedAccount#linked_account.subject_id
            ]),
            LinkedAccounts ++ [LinkedAccount];

        {ok, #linked_account{access_token = OldAccessT, refresh_token = OldRefreshT} = OldLinkedAcc} ->
            CoalescedLinkedAccount = LinkedAccount#linked_account{
                access_token = case NewAccessT of {undefined, _} -> OldAccessT; _ -> NewAccessT end,
                refresh_token = case NewRefreshT of undefined -> OldRefreshT; _ -> NewRefreshT end
            },
            lists_utils:replace(OldLinkedAcc, CoalescedLinkedAccount, LinkedAccounts)
    end,

    UserRecord#od_user{
        emails = NewEmails,
        linked_accounts = NewLinkedAccounts
    }.


-spec normalize_emails([binary()]) -> [binary()].
normalize_emails(Emails) ->
    lists:filtermap(fun(Email) ->
        Normalized = http_utils:normalize_email(Email),
        case http_utils:validate_email(Normalized) of
            true -> {true, Normalized};
            false -> false
        end
    end, Emails).


%% @doc retains the fields used for user mapping in LUMA, stripping unnecessary / private data
-spec apply_luma_info_mask(t()) -> t().
apply_luma_info_mask(LinkedAccount) ->
    LinkedAccount#linked_account{
        full_name = undefined,
        entitlements = []
    }.


%%%===================================================================
%%% jsonable_record API
%%%===================================================================


-spec to_json(t()) -> json_utils:json_map().
to_json(LinkedAccount) ->
    #linked_account{
        idp = IdP,
        subject_id = SubjectId,
        full_name = FullName,
        username = Username,
        emails = Emails,
        entitlements = Entitlements,
        custom = Custom
    } = LinkedAccount,

    #{
        <<"idp">> => IdP,
        <<"subjectId">> => SubjectId,
        <<"fullName">> => utils:undefined_to_null(FullName),
        <<"username">> => utils:undefined_to_null(Username),
        <<"emails">> => Emails,
        <<"entitlements">> => Entitlements,
        <<"custom">> => Custom,

        %% @TODO VFS-4506 deprecated, included for backward compatibility
        <<"name">> => utils:undefined_to_null(FullName),
        <<"login">> => utils:undefined_to_null(Username),
        <<"alias">> => utils:undefined_to_null(Username),
        <<"emailList">> => Emails,
        <<"groups">> => Entitlements
    }.


-spec from_json(json_utils:json_map()) -> t().
from_json(#{
    <<"idp">> := IdP,
    <<"subjectId">> := SubjectId
} = Data) ->
    #linked_account{
        idp = binary_to_existing_atom(IdP, utf8),
        subject_id = SubjectId,
        full_name = utils:null_to_undefined(maps:get(<<"fullName">>, Data, undefined)),
        username = utils:null_to_undefined(maps:get(<<"username">>, Data, undefined)),
        emails = maps:get(<<"emails">>, Data, []),
        entitlements = maps:get(<<"entitlements">>, Data, undefined),
        custom = maps:get(<<"custom">>, Data, undefined)
    }.
