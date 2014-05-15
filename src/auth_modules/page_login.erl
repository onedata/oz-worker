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

-module(page_login).
-compile(export_all).
-include("gui_common.hrl").

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
            #link{target = <<"_blank">>, class = <<"btn btn-small">>,
                style = <<"margin: 10px; text-align: left; width: 200px; background-color: ", ButtonColor/binary>>,
                postback = {auth, HandlerModule}, body = [
                    #span{style = <<"display: inline;">>, body = [
                        #image{image = ButtonIcon, style = <<"margin-right: 10px;">>},
                        ButtonText
                    ]}
                ]}
        end, auth_utils:get_auth_providers()),

    case gui_utils:user_logged_in() of
        true -> wf:redirect(<<"/">>);
        false ->
            ErrorPanelStyle = case wf:q(<<"x">>) of
                                  undefined -> <<"display: none;">>;
                                  _ -> <<"">>
                              end,
            #panel{style = <<"position: relative;">>, body = [
                #panel{id = <<"error_message">>, style = ErrorPanelStyle, class = <<"dialog dialog-danger">>, body = #p{
                    body = <<"Session error or session expired. Please log in again.">>}},
                #panel{class = <<"alert alert-success login-page">>, body = [
                    #h3{body = <<"Welcome to OneData">>},
                    #p{class = <<"login-info">>, body = <<"You can sign in using your account from any of the following.">>},
                    #panel{postback = login, class = <<"btn btn-primary btn-block">>, body = <<"Log in via PL-Grid OpenID">>}
                ]}
            ] ++ gui_utils:logotype_footer(120)}
    end.


event(init) -> ok;
% Login event handling

event({auth, HandlerModule}) ->
    {ok, URL} = HandlerModule:get_redirect_url(),
    % Collect redirect param if present
    RedirectParam = case wf:q(<<"x">>) of
                        undefined -> <<"">>;
                        Val -> <<"?x=", Val/binary>>
                    end,
    % Resolve hostname, which was requested by a client
    Hostname = gui_utils:get_requested_hostname(),
    case Hostname of
        undefined ->
            gui_utils:update("error_message", <<"Cannot establish requested hostname. Please contact the site administrator.">>),
            wf:wire(#jquery{target = "error_message", method = ["fadeIn"], args = [300]});
        Host ->
            % Get redirect URL and redirect to OpenID login
            case openid_utils:get_login_url(Host, RedirectParam) of
                {error, _} ->
                    gui_utils:update("error_message", <<"Unable to reach OpenID Provider. Please try again later.">>),
                    wf:wire(#jquery{target = "error_message", method = ["fadeIn"], args = [300]});
                URL ->
                    wf:redirect(URL)
            end
    end.