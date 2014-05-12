%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: Common definitions for REST handlers.
%% @end
%% ===================================================================
-author("Konrad Zemek").

-ifndef(HANDLERS_REST_HANDLER_HRL).
-define(HANDLERS_REST_HANDLER_HRL, true).

%% The state of the request.
%% `module` :: module() is the identifier of the REST module handling resuest
%% details.
%% `resource` :: atom() is the name of the requested resource.
%% `client` :: {user | provider, binary()} is the requester identified by his
%% type and id.
-record(reqstate, {
    module,
    resource,
    client
}).

-endif. %% HANDLERS_REST_HANDLER_HRL
