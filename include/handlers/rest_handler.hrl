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


%% A type describing atoms relating to REST methods.
-type method() :: post | patch | get | put | delete.


%% A description of REST request's client.
%% `type` is the client's type.
%% `id` is the client's ID in the database.
-record(client, {
    type :: user | provider,
    id :: binary()
}).
-type client() :: #client{}.


%% The options associated with a particular request.
%% `module` is the identifier of the REST module handling request's details.
%% `resource` is the name of the requested resource.
%% `methods` is an array of REST methods the resource accepts.
%% `client` is the authenticated client's data.
-record(rstate, {
    module :: module(),
    resource :: atom(),
    methods :: [method()],
    client :: client()
}).
-type rstate() :: #rstate{}.


-endif. %% HANDLERS_REST_HANDLER_HRL
