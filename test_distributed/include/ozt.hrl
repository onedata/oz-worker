%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% Common definitions and includes for ozt* modules.
%%% @end
%%%-------------------------------------------------------------------
-author("Lukasz Opiola").

-ifndef(OZT_HRL).
-define(OZT_HRL, 1).

-include("api_test_utils.hrl").
-include("entity_logic.hrl").
-include("graph_sync/oz_graph_sync.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/http/headers.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("gui/include/gui_session.hrl").
-include_lib("cluster_worker/include/graph_sync/graph_sync.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").

% Time caveat is required in temporary tokens, a default one is added if there isn't any
-define(DEFAULT_TEMP_CAVEAT_TTL, 36000).

-define(RAND_REV_NUMBER(), ?RAND_INT(1, 100)).

% Macro used to check the result in ensure_exists / ensure_member functions
% (where an entity / relation should be created if it does not exist)
-define(assertSuccessOrAlreadyExists(Result), ?assertMatch(ok, case Result of
    ok -> ok;
    {ok, _} -> ok;
    ?ERROR_ALREADY_EXISTS -> ok;
    ?ERR_RELATION_ALREADY_EXISTS(_, _, _, _) -> ok;
    Other -> Other
end)).

% Macro used to check the result in ensure_does_not_exist / ensure_not_a_member functions
% (where an entity / relation should be removed if it exists)
-define(assertSuccessOrDoesNotExist(Result), ?assertMatch(ok, case Result of
    ok -> ok;
    {ok, _} -> ok;
    ?ERROR_NOT_FOUND -> ok;
    ?ERR_RELATION_DOES_NOT_EXIST(_, _, _, _) -> ok;
    Other -> Other
end)).

% Macro useful for debugging
-define(wrap_in_try_catch(Term), try
    Term
catch __Type:__Reason:__Stacktrace ->
    ?ct_pal_exception(?autoformat_with_msg(
        "Test crash in", [?MODULE, ?LINE]
    ), __Type, __Reason, __Stacktrace),
    error(test_crashed)
end).

-endif.
