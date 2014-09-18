%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains n2o website code - the homepage of GR,
%% with a customisable section.
%% @end
%% ===================================================================

-module(page_login).

-include("gui/common.hrl").
-include("auth_common.hrl").
-include_lib("ctool/include/logging.hrl").

% n2o API
-export([main/0, event/1]).

-define(HOME_PAGE_FILE, "resources/HOMEPAGE_HEADER.html").

-define(ONEDATA_INFO, "http://slides.com/onedata/globalstorage#/").

%% Template points to the template file, which will be filled with content
main() ->
    #dtl{file = "bare", app = ?APP_Name, bindings = [{title, title()}, {body, body()}, {custom, <<"">>}]}.


%% Page title
title() -> <<"onedata homepage">>.


%% This will be placed in the template instead of {{body}} tag
body() ->
    case gui_ctx:user_logged_in() of
        true ->
            gui_jq:redirect(<<"/">>);
        false ->
            ErrorPanelStyle = case gui_ctx:url_param(<<"x">>) of
                                  undefined -> <<"display: none;">>;
                                  _ -> <<"">>
                              end,
            #panel{style = <<"padding: 20px 50px;">>, body = [
                #panel{id = <<"error_message">>, style = ErrorPanelStyle, class = <<"dialog dialog-danger">>, body = #p{
                    body = <<"Session error or session expired. Please log in again.">>}},
                #panel{body = homepage_header()},
                login_panel()
            ]}
    end.

login_panel() ->
    Referer = gui_ctx:url_param(<<"referer">>),
    case auth_config:get_auth_providers() of
        [] ->
            % Render error message that no auth providers were specified
            #panel{style = <<"position: relative;">>, body = [
                #panel{id = <<"error_message">>, style = <<"margin: 20px 10px;">>, class = <<"dialog dialog-danger">>, body = #p{
                    body = <<"Authentication config could not be loaded. Please make sure that file <b>",
                    ?auth_config_file_path, "</b> contains correct config for at least one OAuth/OpenID provider.">>}}
            ]};
        ProviderList ->
            Buttons = lists:map(
                fun(Provider) ->
                    ButtonText = <<"Sign in with ", (auth_config:get_provider_name(Provider))/binary>>,
                    ButtonIcon = auth_config:get_provider_button_icon(Provider),
                    ButtonColor = auth_config:get_provider_button_color(Provider),
                    HandlerModule = auth_config:get_provider_module(Provider),
                    #link{id = <<"login_", (atom_to_binary(Provider, latin1))/binary>>, class = <<"btn btn-small">>, postback = {auth, HandlerModule, Referer},
                        style = <<"margin: 10px; text-align: left; width: 200px; background-color: ", ButtonColor/binary>>,
                        body = [
                            #span{style = <<"display: inline-block; line-height: 32px;">>, body = [
                                #image{image = ButtonIcon, style = <<"margin-right: 10px;">>},
                                ButtonText
                            ]}
                        ]}
                end, ProviderList),

            #panel{id = <<"login_panel">>, style = <<"position: relative;">>, body = [
                #panel{class = <<"alert alert-success login-page">>, body = [
                    #h3{id = <<"main_header">>, body = <<"Welcome to onedata">>},
                    #p{class = <<"login-info">>, body = <<"You can sign in using one of your existing accounts.">>},
                    #panel{body = Buttons},
                    #hr{style = <<"border-color: #E0EAEB;">>},
                    #h3{style = <<"margin-top: 35px;">>, body = <<"Learn more">>},
                    #link{class = <<"btn btn-success">>, url = <<?ONEDATA_INFO>>,
                        style = <<"margin: 10px; width: 200px;">>,
                        body = <<"What is onedata?">>},
                    #link{class = <<"btn btn-success">>, url = <<?become_a_provider_url>>,
                        style = <<"margin: 10px; width: 200px;">>,
                        body = <<"Become a provider">>}
                ]},
                gui_utils:cookie_policy_popup_body(<<?privacy_policy_url>>)
            ] ++ gr_gui_utils:logotype_footer(120)}
    end.


% content of HOMEPAGE_HEADER.html file
homepage_header() ->
    case file:read_file(?HOME_PAGE_FILE) of
        {ok, File} -> File;
        {error, _Error} -> <<"">>
    end.


event(init) ->
    ok;

event(terminate) ->
    ok;

% Login event handling
event({auth, HandlerModule, Referer}) ->
    %% TODO for development purposes
    %% remember which provider redirected the client to GR
    erlang:put(referer, Referer),
    {ok, URL} = HandlerModule:get_redirect_url(false),
    gui_jq:redirect(URL).