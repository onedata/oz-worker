%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% Definitions of macros and records used in subscriptions tests.
%%% @end
%%%-------------------------------------------------------------------
-author("Michal Zmuda").
-author("Jakub Kudzia").

-define(MESSAGES_WAIT_TIMEOUT, timer:seconds(2)).
-define(MESSAGES_RECEIVE_ATTEMPTS, 60).


%% appends function name to id (atom) and yields binary accepted by the db
-define(ID(Id), list_to_binary(atom_to_list(Id) ++ " # " ++ atom_to_list(
    element(2, element(2, process_info(self(), current_function)))))
).

%% helper record for maintaining subscription progress between message receives
-record(subs_ctx, {
    node :: node(),
    provider :: binary(),
    users :: [binary()],
    resume_at :: subscriptions:seq(),
    missing :: [subscriptions:seq()]
}).

-define(ATOM_FORMAT(Format, Data),
    list_to_atom(str_utils:format(Format, Data))).

-define(PROVIDERS_NUM(Value), [
    {name, providers_num},
    {value, Value},
    {description, "Number of providers (threads) used during the test."}
]).

-define(DOCS_NUM(Value), [
    {name, docs_num},
    {value, Value},
    {description, "Number of documents used by a single thread/provider."}
]).

-define(DOCUMENT_MODIFICATIONS_NUM(Value), [
    {name, docs_modifications_num},
    {value, Value},
    {description, "Number of modifications on document, performed by a single thread/provider."}
]).

-define(USERS_NUM(Value), [
    {name, users_num},
    {value, Value},
    {description, "Number of users."}
]).

-define(GROUPS_NUM(Value), [
    {name, groups_num},
    {value, Value},
    {description, "Number of groups."}
]).


-define(GENERATE_TEST_CFG(ProvidersNum, DocumentsNum), {config,
    [
        {name, ?ATOM_FORMAT("~p providers ~p documents", [ProvidersNum, DocumentsNum])},
        {description, str_utils:format("~p providers saving ~p documents", [ProvidersNum, DocumentsNum])},
        {parameters, [
            ?PROVIDERS_NUM(ProvidersNum),
            ?DOCS_NUM(DocumentsNum)
        ]}
    ]
}).

-define(UPDATE_TEST_CFG(ModificationsNum, UsersNum, GroupsNum), {config,
    [
        {name, ?ATOM_FORMAT("~p modifications ~p users ~p groups", [ModificationsNum, UsersNum, GroupsNum])},
        {description, str_utils:format("Single provider modifying document ~p times, ~p users, ~p group", [ModificationsNum, UsersNum, GroupsNum])},
        {parameters, [
            ?DOCUMENT_MODIFICATIONS_NUM(ModificationsNum),
            ?USERS_NUM(UsersNum),
            ?GROUPS_NUM(GroupsNum)
        ]}
    ]
}).
