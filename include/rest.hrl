%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Common definitions for REST
%%% @end
%%%-------------------------------------------------------------------

-ifndef(REST_CONFIG_HRL).
-define(REST_CONFIG_HRL, 1).

-define(REST_LISTENER, rest).
-define(REST_HANDLER_MODULE, n_rest_handler).

%% Record containing the state of REST request.
-record(rest_req, {
    % tODO strict typy
    methods = #{} :: #{Method :: get => {data_plugin, entityid, resource}},
    % To jest wywnioskowane z methods
    handler = {module, function, [args]}
}).
% Convenience macros user in rest_req
-define(BINDING(__KEY), {binding, __KEY}).
-define(CLIENT_ID, client_id).

-endif.