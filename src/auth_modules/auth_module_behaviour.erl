%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This behaviour should be implemented by modules handling OpenID and OAuth requests.
%% It ensures the presence of required callbacks.
%% @end
%% ===================================================================
-module(auth_module_behaviour).

-export([behaviour_info/1]).

%% behaviour_info/1
%% ====================================================================
%% @doc Defines the behaviour (lists the callbacks and their arity)
-spec behaviour_info(Arg) -> Result when
    Arg :: callbacks | Other,
    Result :: [Fun_def] | undefined,
    Fun_def :: tuple(),
    Other :: any().
%% ====================================================================
behaviour_info(callbacks) ->
    [
        {get_redirect_url, 1},
        {validate_login, 1}
    ];

behaviour_info(_Other) ->
    undefined.

%% ====================================================================
%% Callbacks descriptions
%% ====================================================================

%% TODO Wziac z rest_module_beh