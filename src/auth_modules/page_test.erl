%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2013 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains n2o website code
%% @end
%% ===================================================================

-module(page_test).
-compile(export_all).
-include("gui_common.hrl").
-include("logging.hrl").

%% Template points to the template file, which will be filled with content
main() -> #dtl{file = "bare", app = veil_cluster_node, bindings = [{title, title()}, {body, body()}]}.

%% Page title
title() -> <<"Login page">>.

%% This will be placed in the template instead of {{body}} tag
body() ->
    Buttons = lists:map(
        fun(Provider) ->
            ButtonText = auth_utils:get_provider_button_text(Provider),
            ButtonIcon = auth_utils:get_provider_button_icon(Provider),
            ButtonColor = auth_utils:get_provider_button_color(Provider),
            HandlerModule = auth_utils:get_provider_module(Provider),
            {ok, URL} = HandlerModule:get_redirect_url(),
            #link{target = <<"_blank">>, class = <<"btn btn-small">>, style = <<"text-align: left; width: 200px; background-color: ", ButtonColor/binary>>,
                url = URL, body = [
                    #span{style = <<"display: inline;">>, body = [
                        #image{image = ButtonIcon, style = <<"margin-right: 10px;">>},
                        ButtonText
                    ]}
                ]}
        end, auth_utils:get_auth_providers()),
    #panel{body = Buttons}.

event(init) -> ok.