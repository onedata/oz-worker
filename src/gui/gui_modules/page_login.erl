%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc: This file contains n2o website code - the homepage of GR,
%%% with a customisable section.
%%% @end
%%%-------------------------------------------------------------------

-module(page_login).

-include("registered_names.hrl").
-include("gui/common.hrl").
-include("auth_common.hrl").
-include("dao/dao_users.hrl").
-include("dao/dao_external.hrl").
-include_lib("dao/include/dao_helper.hrl").
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
    % If dev_mode is on, include a logging menu that allows to log on any account.
    DevLoginPanel = case application:get_env(?APP_Name, dev_mode) of
                        {ok, true} ->
                            {ok, UserIDs} = onedata_user:get_all_ids(),
                            #panel{style = <<"text-align: center; margin-bottom: 100px;">>, body = [
                                #h2{body = <<"GlobalRegistry works in DEV MODE">>},
                                #p{body = <<"You can use the menu below to login on user accounts, bypassing OpenID">>},
                                #h5{body = <<"dev login:">>},
                                lists:map(
                                    fun(UserID) ->
                                        #panel{style = <<"width: 100%; margin: 10px 0;">>,
                                            body = #link{class = <<"btn btn-inverse">>, style = <<"width: 200px;">>,
                                            postback = {dev_login, UserID}, body = UserID}}
                                    end, UserIDs),
                                #hr{},
                                #p{body = <<"Or log in normally:">>}
                            ]};
                        _ ->
                            []
                    end,

    case gui_ctx:user_logged_in() of
        true ->
            gui_jq:redirect(<<"/">>);
        false ->
            #panel{style = <<"padding: 20px 50px;">>, body = [
                DevLoginPanel,
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
                        style = <<"margin: 10px; text-align: left; width: 210px; background-color: ", ButtonColor/binary>>,
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
                        style = <<"margin: 10px; width: 210px;">>,
                        body = <<"What is <strong>onedata</strong>?">>},
                    #link{class = <<"btn btn-success">>, url = <<?become_a_provider_url>>,
                        style = <<"margin: 10px; width: 210px;">>,
                        body = <<"Become a provider">>}
                ]},
                #link{postback = show_copernicus_info, body = #image{image = "/images/copernicus.png",
                    style = <<"display: block; margin-left: auto; margin-right: auto; margin-top: 75px; width: 300px;">>}},
                gui_utils:cookie_policy_popup_body(<<?privacy_policy_url>>)
            ] ++ gr_gui_utils:logotype_footer(55)}
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

% Developer login event handling
event({dev_login, UserID}) ->
    gui_jq:redirect(<<"validate_dev_login?user=", UserID/binary>>);

% Login event handling
event({auth, HandlerModule, Referer}) ->
    %% TODO for development purposes
    %% remember which provider redirected the client to GR
    erlang:put(referer, Referer),
    {ok, URL} = HandlerModule:get_redirect_url(false),
    gui_jq:redirect(URL);

event(show_copernicus_info) ->
    Text = <<
    "EN<br />A team from AGH UST in Cracow, formed of Department of Computer Science "/utf8,
    "and ACC Cyfronet workers, realising <b>onedata</b> system as part of "/utf8,
    "PL-Grid Plus and PL-Grid Core projects, has been <b>awarded 3rd place</b> in "/utf8,
    "\"T-SYSTEMS BIG DATA CHALLENGE\". Copernicus is an Earth monitoring "/utf8,
    "programme, headed by the European Commission (EC). Since 2011, "/utf8,
    "the annual Copernicus Masters competition is awarding prizes to innovative "/utf8,
    "solutions for business and society based on Earth observation data.<br /><br />"/utf8,

    "PL<br />Zespół Katedry Informatyki i ACK Cyfronet AGH realizujacy system "/utf8,
    "<b>onedata</b>, w ramach projektów: PL-Grid Plus i PL-Grid Core, <b>zajął 3. miejsce</b> "/utf8,
    "w konkursie \"T-SYSTEMS BIG DATA CHALLENGE\". Copernicus to program "/utf8,
    "monitorowania Ziemi realizowany przez Komisję Europejską. Od roku 2011, "/utf8,
    "corocznie, odbywa się konkurs Copernicus Masters w ramach którego "/utf8,
    "przyznawane są nagrody za innowacyjne, dla gospodarki i społeczeństwa, "/utf8,
    "rozwiązania związane z obsługą danych otrzymywanych z obserwacji ziemi. "/utf8>>,
    gui_jq:info_popup(<<"Onedata in Copernicus Masters">>, Text, <<"">>).

