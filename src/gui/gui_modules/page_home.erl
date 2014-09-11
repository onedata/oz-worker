%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2013 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains n2o website code - the homepage of GR,
%% with a customisable section.
%% @end
%% ===================================================================

-module(page_home).

-include("gui/common.hrl").
-include("auth_common.hrl").
-include_lib("ctool/include/logging.hrl").

% n2o API
-export([main/0, event/1]).

-define(HOME_PAGE_FILE, "resources/HOMEPAGE_HEADER.html").

-define(DOWNLOAD_LINK, "http://packages.onedata.org/VeilCluster-Linux.rpm").

%% Template points to the template file, which will be filled with content
main() ->
    #dtl{file = "bare", app = veil_cluster_node, bindings = [{title, title()}, {body, body()}, {custom, <<"">>}]}.


%% Page title
title() -> <<"onedata homepage">>.


%% This will be placed in the template instead of {{body}} tag
body() ->
    case gui_ctx:user_logged_in() of
        true ->
            gui_jq:redirect(<<"/">>);
        false ->
            #panel{style = <<"padding: 20px 50px;">>, body = [
                #panel{body = homepage_header()},
                about_provider()
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
                    #link{class = <<"btn btn-small">>, postback = {auth, HandlerModule, Referer},
                        style = <<"margin: 10px; text-align: left; width: 200px; background-color: ", ButtonColor/binary>>,
                        body = [
                            #span{style = <<"display: inline-block; line-height: 32px;">>, body = [
                                #image{image = ButtonIcon, style = <<"margin-right: 10px;">>},
                                ButtonText
                            ]}
                        ]}
                end, ProviderList),

            ErrorPanelStyle = case gui_ctx:url_param(<<"x">>) of
                                  undefined -> <<"display: none;">>;
                                  _ -> <<"">>
                              end,

            #panel{id = <<"main_panel">>, style = <<"position: relative;">>, body = [
                #panel{id = <<"error_message">>, style = ErrorPanelStyle, class = <<"dialog dialog-danger">>, body = #p{
                    body = <<"Session error or session expired. Please log in again.">>}},
                #panel{class = <<"alert alert-success login-page">>, body = [
                    #h3{id = <<"main_header">>, body = <<"Welcome to onedata">>},
                    #p{class = <<"login-info">>, body = <<"You can sign in using one of your existing accounts.">>},
                    #panel{style = <<"">>, body = Buttons},
                    #hr{style = <<"border-color: #E0EAEB;">>},
                    #h3{style = <<"margin-top: 35px;">>, body = <<"Become a provider">>},
                    #p{class = <<"login-info">>, body = <<"Deploy our system on your servers to be a part of <strong>onedata</strong>.">>},
                    #link{class = <<"btn btn-success">>, postback = read_about_provider,
                        style = <<"margin: 10px 10px 25px; text-align: center; width: 446px;">>,
                        body = [
                            <<"Read more">>,
                            <<"<i class=\"fui-arrow-right pull-right\"></i>">>
                    ]}
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


about_provider() ->
    #panel{id = <<"main_panel">>, style = <<"position: relative;">>, body = [
        #panel{class = <<"alert alert-success login-page">>, body = [
            #h3{id = <<"main_header">>, body = <<"Become a provider">>},
            #p{class = <<"login-info">>, body = <<"<strong>Provider</strong> in <strong>onedata</strong> ",
            "is anyone who decides to install our software on his servers. After the process, he can ",
            "support storage spaces for onedata users.">>},
            #h6{style = <<"margin-top: 30px;">>, body = <<"Installation">>},
            #list{numbered = true, style = <<"text-align: left;">>, body = [
                #li{body = [
                    <<"Download the RPM package from ">>,
                    #link{body = <<"here">>, url = <<?DOWNLOAD_LINK>>},
                    <<".">>
                ]},

                #li{body = <<"Install the package on every server in the cluster ",
                "you would like to deploy the software on. Minimal cluster size is 1 machine, although ",
                "it is <strong>strongly recommended</strong> to use at least 4 machines.">>},

                #li{body = <<"After completing the installation on every node, visit ",
                "<strong>&lthostname&gt:9443</strong>, where &lthostname&gt is any of the nodes.">>},

                #li{body = <<"Follow the installation instructions that appear in the browser.">>},

                #li{body = <<"When you are prompted to register at <strong>Global Registry</strong>, do so.">>},

                #li{body = <<"Congratulations, you are now a <strong>onedata provider</strong>!">>}

            ]},
            #link{class = <<"btn btn-success">>, postback = back_to_homepage,
                style = <<"margin: 10px 10px 25px; text-align: center; width: 446px;">>,
                body = [
                    <<"Back to home page">>,
                    <<"<i class=\"fui-arrow-right pull-right\"></i>">>
                ]}
        ]},
        gui_utils:cookie_policy_popup_body(<<?privacy_policy_url>>)
    ] ++ gr_gui_utils:logotype_footer(120)}.


event(init) ->
    ok;

event(terminate) ->
    ok;

event(read_about_provider) ->
    gui_jq:replace(<<"main_panel">>, about_provider());

event(back_to_homepage) ->
    gui_jq:replace(<<"main_panel">>, login_panel()).

