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
            ErrorPanelStyle = case gui_ctx:url_param(<<"x">>) of
                                  undefined -> <<"display: none;">>;
                                  _ -> <<"">>
                              end,
            #panel{style = <<"padding: 20px 50px;">>, body = [
                #panel{id = <<"error_message">>, style = ErrorPanelStyle, class = <<"dialog dialog-danger">>, body = #p{
                    body = <<"Session error or session expired. Please log in again.">>}},
                #panel{body = homepage_header()},
                login_panel(),
                about_provider_panel()
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
                    #panel{style = <<"">>, body = Buttons},
                    #hr{style = <<"border-color: #E0EAEB;">>},
                    #h3{style = <<"margin-top: 35px;">>, body = <<"Become a provider">>},
                    #p{class = <<"login-info">>, body = <<"Deploy our system on your servers to be a part of <strong>onedata</strong>.">>},
                    #link{id = <<"to_provider">>, class = <<"btn btn-success">>, postback = {toggle_page, provider},
                        style = <<"margin: 10px 10px 25px; width: 446px;">>,
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


about_provider_panel() ->
    #panel{id = <<"about_provider_panel">>, style = <<"position: relative; display: none;">>, body = [
        #panel{class = <<"alert alert-success login-page">>, body = [
            #h3{id = <<"main_header">>, body = <<"Become a provider">>},
            #p{class = <<"login-info">>, body = <<"<strong>Provider</strong> in <strong>onedata</strong> ",
            "is anyone who decides to install our software on his servers. After the process, he can ",
            "support storage spaces for onedata users.">>},
            #h6{style = <<"margin-top: 30px;">>, body = <<"Installation steps">>},
            #list{numbered = true, style = <<"text-align: left;">>, body = [
                #li{style = <<"margin: 9px 0;">>, body = <<"Prepare a cluster of machines on which you",
                " would like to deploy the software. It can run on just 1 machine, although it is ",
                "<strong>strongly recommended</strong> to use at least 4 machines for performance reasons.">>},

                #li{style = <<"margin: 9px 0;">>, body = <<"The software is intended for Red Hat based systems. ",
                "It has been thoroughly tested on Scientific Linux.">>},

                #li{style = <<"margin: 9px 0;">>, body = <<"All hosts in the cluster must be visible to each ",
                "other under unique, fully qualified hostnames.">>},

                #li{style = <<"margin: 9px 0;">>, body = [
                    <<"Download the RPM package from ">>,
                    #link{body = <<"here">>, url = <<?DOWNLOAD_LINK>>},
                    <<".">>
                ]},

                #li{style = <<"margin: 9px 0;">>, body = <<"Install the package on every node in the cluster ",
                "you would like to deploy the software on. ">>},

                #li{style = <<"margin: 9px 0;">>, body = <<"After completing the installation, ",
                "visit <strong>onepanel</strong> which is an admin panel for the cluster. It is hosted on every",
                " node under <strong>https://&lthostname&gt:9443</strong>. You can use any of the nodes.">>},

                #li{style = <<"margin: 9px 0;">>, body = <<"Follow the installation instructions that appear in the browser.">>},

                #li{style = <<"margin: 9px 0;">>, body = <<"When you are prompted to register as a provider, do so.">>},

                #li{style = <<"margin: 9px 0;">>, body = <<"Congratulations, you are now a <strong>onedata provider</strong>!">>},

                #li{style = <<"margin: 9px 0;">>, body = <<"Use <strong>onepanel</strong> to grant support for spaces ",
                "(Spaces -> Settings). You will need a token from a space owner to do so. To try it for yourself, ",
                "log in to <strong>onedata</strong> as a user to aquire such token.">>}
            ]},
            #link{class = <<"btn btn-primary">>, url = <<?DOWNLOAD_LINK>>,
                style = <<"margin: 25px 10px 10px; width: 446px;">>,
                body = <<"Download RPM">>},
            #link{id = <<"to_login">>, class = <<"btn btn-success">>, postback = {toggle_page, login},
                style = <<"margin: 15px 10px 25px; width: 446px;">>,
                body = <<"Back to login page">>}
        ]},
        gui_utils:cookie_policy_popup_body(<<?privacy_policy_url>>)
    ] ++ gr_gui_utils:logotype_footer(120)}.


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
    gui_jq:redirect(URL);

event({toggle_page, Type}) ->
    {Hide, Show} = case Type of
                       login -> {<<"about_provider_panel">>, <<"login_panel">>};
                       provider -> {<<"login_panel">>, <<"about_provider_panel">>}
                   end,
    gui_jq:hide(Hide),
    gui_jq:fade_in(Show, 150).
