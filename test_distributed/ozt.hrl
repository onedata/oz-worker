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
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/http/headers.hrl").
-include_lib("gui/include/gui_session.hrl").
-include_lib("cluster_worker/include/graph_sync/graph_sync.hrl").

% Time caveat is required in temporary tokens, a default one is added if there isn't any
-define(DEFAULT_TEMP_CAVEAT_TTL, 36000).

% Macro used to check the result in ensure_member functions
% (where a relation should be added if it does not exist)
-define(assertSuccessOrRelationExists(Result), ?assertMatch(ok, case Result of
    {ok, _} -> ok;
    ?ERROR_RELATION_ALREADY_EXISTS(_, _, _, _) -> ok;
    Other -> Other
end)).

% Macro used to check the result in ensure_not_a_member functions
% (where a relation should be removed if it exists)
-define(assertSuccessOrNoSuchRelation(Result), ?assertMatch(ok, case Result of
    ok -> ok;
    ?ERROR_NOT_FOUND -> ok;
    ?ERROR_RELATION_DOES_NOT_EXIST(_, _, _, _) -> ok;
    Other -> Other
end)).

-endif.
