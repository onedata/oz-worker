%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains n2o website code
%% @end
%% ===================================================================

-module(page_auth_endpoint).
-compile(export_all).
-include("auth_common.hrl").
-include("logging.hrl").
-include("gui_common.hrl").

main() ->
    AuthorizationCode = wf:q(<<"authorization_code">>),
    UserGlobalID = temp_user_logic:get_association(AuthorizationCode),
    temp_user_logic:get_user_to_json(UserGlobalID).