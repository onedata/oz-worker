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


%% A subset of [proplists:property()] that allows for easy encoding to - and
%% decoding from JSON.
-type reqdata() :: [{binary() | atom(), binary() | atom() | integer()}].


%% A description of REST request's client.
%% `type` is the client's type.
%% `id` is the client's ID in the database.
-record(reqclient, {
    type :: user | provider,
    id :: binary()
}).


%% The state of the request.
%% `module` is the identifier of the REST module handling request's details.
%% `resource` is the name of the requested resource.
%% `client` is the request's client (the requester).
%% `data` is the parsed data submitted by the client as a part of the request.
%% `resid` is the id of the resource if found in the resource's address.
-record(reqstate, {
    module :: module(),
    resource :: atom(),
    client :: #reqclient{},
    data :: reqdata(),
    resid :: binary()
}).

-endif. %% HANDLERS_REST_HANDLER_HRL
