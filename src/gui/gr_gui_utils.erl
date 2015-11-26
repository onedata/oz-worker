%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2013 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc: This file contains useful functions commonly used in
%%% globalregistry GUI modules.
%%% @end
%%%-------------------------------------------------------------------

-module(gr_gui_utils).
-include("gui/common.hrl").
-include("dao/dao_types.hrl").
-include_lib("ctool/include/logging.hrl").

% Functions to check for user's session
-export([apply_or_redirect/2, apply_or_redirect/3, maybe_redirect/1]).

% Handling redirects to providers
-export([get_redirection_url_to_provider/1]).

% Functions to generate page elements
-export([top_menu/1, top_menu/2, logotype_footer/1, empty_page/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Checks if the client has right to do the operation (is logged in). If so, it executes the code.
%% @end
%%--------------------------------------------------------------------
-spec apply_or_redirect(Module :: atom, Fun :: atom) -> boolean().
apply_or_redirect(Module, Fun) ->
    apply_or_redirect(Module, Fun, []).

%%--------------------------------------------------------------------
%% @doc Checks if the client has right to do the operation (is logged in). If so, it executes the code.
%% @end
%%--------------------------------------------------------------------
-spec apply_or_redirect(Module :: module(), Fun :: atom(), Args :: [term()]) -> term() | no_return.
apply_or_redirect(Module, Fun, Args) ->
    try
        case gui_ctx:user_logged_in() of
            false ->
                gui_jq:redirect_to_login();
            true ->
                erlang:apply(Module, Fun, Args)
        end
    catch Type:Message ->
        ?error_stacktrace("Error in ~p - ~p:~p", [Module, Type, Message]),
        page_error:redirect_with_error(?error_internal_server_error),
        case gui_comet:is_comet_process() of
            true ->
                gui_comet:flush();
            false ->
                ok
        end
    end.

%%--------------------------------------------------------------------
%% @doc Decides if user can view the page, depending on arguments.
%% Returns false if no redirection is needed.
%% Otherwise, it issues a redirection and returns true.
%% Setting "SaveSourcePage" on true will allow a redirect back from login.
%% NOTE: Should be called from page:main().
%% @end
%%--------------------------------------------------------------------
-spec maybe_redirect(NeedLogin :: boolean()) -> boolean().
maybe_redirect(NeedLogin) ->
    case NeedLogin and (not gui_ctx:user_logged_in()) of
        true ->
            gui_jq:redirect_to_login(),
            true;
        false ->
            false
    end.

%%--------------------------------------------------------------------
%% @doc Returns an URL that the user should be redirected to - if possible.
%% Otherwise, error is returned.
%% If the referer is known (the provider who redirected the user for login),
%% then he will be chosen with highest priority.
%% @end
%%--------------------------------------------------------------------
-spec get_redirection_url_to_provider(Referer :: binary() | undefined) ->
    {ok, ProviderHostname :: binary(), URL :: binary()} | {error, Desc :: no_provider | term()}.
get_redirection_url_to_provider(Referer) ->
    try
        UserID = gui_ctx:get_user_id(),

        % Default provider is the provider that redirected the user for login.
        % Check if the provider is recognisable
        RefererData =
            try
                {ok, Data} = provider_logic:get_data(Referer),
                Data
            catch _:_ ->
                undefined
            end,

        {ProviderID, ProviderData} = case RefererData of
                                         undefined ->
                                             % If referer isn't recognizable, look for any provider
                                             case provider_logic:get_default_provider_for_user(UserID) of
                                                 {error, no_provider} ->
                                                     {undefined, undefined};
                                                 {ok, ProvID} ->
                                                     {ok, PData} = provider_logic:get_data(ProvID),
                                                     {ProvID, PData}
                                             end;
                                         _ ->
                                             % Referer is OK
                                             {Referer, RefererData}
                                     end,

        % If any provider was found, get redirection uri
        case ProviderID of
            undefined ->
                {error, no_provider};
            _ ->
                RedirectionPoint = proplists:get_value(redirectionPoint, ProviderData),
                {ok, {_Scheme, _UserInfo, _HostStr, Port, _Path, _Query}} = http_uri:parse(str_utils:to_list(RedirectionPoint)),
                {ok, RedirectURI} = auth_logic:get_redirection_uri(UserID, ProviderID, Port),
                {ok, RedirectionPoint, RedirectURI}
        end
    catch T:M ->
        ?error_stacktrace("Cannot resolve redirection URL to provider - ~p:~p", [T, M]),
        {error, no_provider}
    end.

%%--------------------------------------------------------------------
%% @doc Convienience function to render top menu in GUI pages. 
%% Item with ActiveTabID will be highlighted as active.
%% @end
%%--------------------------------------------------------------------
-spec top_menu(ActiveTabID :: term()) -> list().
top_menu(ActiveTabID) ->
    top_menu(ActiveTabID, []).

%%--------------------------------------------------------------------
%% @doc Convienience function to render top menu in GUI pages.
%% Item with ActiveTabID will be highlighted as active.
%% Submenu body (list of n2o elements) will be concatenated below the main menu.
%% @end
%%--------------------------------------------------------------------
-spec top_menu(ActiveTabID :: term(), SubMenuBody :: term()) -> list().
top_menu(ActiveTabID, SubMenuBody) ->
    % Define menu items with ids, so that proper tab can be made active via function parameter
    {ok, #user{name = Name}} = user_logic:get_user(gui_ctx:get_user_id()),

    MenuCaptions = [],

    MenuIcons =
        [
            {manage_account_tab, #li{body = #link{style = <<"padding: 13px 11px 14px;">>, title = <<"Manage account">>,
                url = <<"/manage_account">>, body = [
                    #panel{style = <<"line-height: 24px; height: 24px;">>, body = [
                        #span{style = <<"display: inline; font-size: 15px; vertical-align:middle;">>, body = Name},
                        #span{class = <<"icomoon-user">>, style = <<"margin-left: 10px; font-size: 24px; vertical-align:middle;">>}
                    ]}
                ]}}},
            {about_tab, #li{body = #link{style = <<"padding: 14px 13px;">>, title = <<"About">>,
                url = <<"/about">>, body = #span{class = <<"icomoon-info2">>, style = <<"font-size: 24px;">>}}}},
            {logout_button, #li{body = #link{style = <<"padding: 14px 13px;">>, title = <<"Log out">>,
                url = <<"/logout">>, body = #span{class = <<"icomoon-switch">>, style = <<"font-size: 24px;">>}}}}
        ],

    MenuCaptionsProcessed = lists:map(
        fun({TabID, ListItem}) ->
            case TabID of
                ActiveTabID -> ListItem#li{class = <<"active">>};
                _ -> ListItem
            end
        end, MenuCaptions),

    MenuIconsProcessed = lists:map(
        fun({TabID, ListItem}) ->
            case TabID of
                ActiveTabID -> ListItem#li{class = <<"active">>};
                _ -> ListItem
            end
        end, MenuIcons),

    gui_jq:wire(<<"initialize_top_menu();">>),
    [
        #panel{id = <<"top_menu">>, class = <<"navbar navbar-fixed-top">>, body = [
            #panel{class = <<"navbar-inner">>, style = <<"border-bottom: 2px solid gray;">>, body = [
                #panel{class = <<"container">>, body = [
                    #list{class = <<"nav pull-left">>, body = MenuCaptionsProcessed},
                    #list{class = <<"nav pull-right">>, body = MenuIconsProcessed}
                ]}
            ]}
        ] ++ SubMenuBody}
    ] ++ gui_utils:cookie_policy_popup_body(<<?privacy_policy_url>>).

%%--------------------------------------------------------------------
%% @doc Convienience function to render logotype footer, coming after page content.
%% @end
%%--------------------------------------------------------------------
-spec logotype_footer(MarginTop :: integer()) -> list().
logotype_footer(MarginTop) ->
    Height = integer_to_binary(MarginTop + 82),
    Margin = integer_to_binary(MarginTop),
    [
        #panel{style = <<"position: relative; height: ", Height/binary, "px;">>, body = [
            #panel{style = <<"text-align: center; z-index: -1; margin-top: ", Margin/binary, "px;">>, body = [
                #image{style = <<"margin: 10px 100px;">>, image = <<"/images/innow-gosp-logo.png">>},
                #image{style = <<"margin: 10px 100px;">>, image = <<"/images/plgrid-plus-logo.png">>},
                #image{style = <<"margin: 10px 100px;">>, image = <<"/images/unia-logo.png">>}
            ]}
        ]}
    ].

%%%===================================================================
%%% Development functions
%%%===================================================================

empty_page() ->
    [
        #h6{body = <<"Not yet implemented">>},
        #br{}, #br{}, #br{}, #br{}, #br{},
        #br{}, #br{}, #br{}, #br{}, #br{},
        #br{}, #br{}, #br{}, #br{}, #br{},
        #br{}, #br{}, #br{}, #br{}, #br{},
        #br{}, #br{}, #br{}, #br{}, #br{},
        #br{}, #br{}, #br{}, #br{}, #br{},
        #br{}, #br{}, #br{}, #br{}, #br{},
        #br{}, #br{}, #br{}, #br{}, #br{},
        #br{}, #br{}, #br{}, #br{}, #br{},
        #br{}, #br{}, #br{}, #br{}, #br{},
        #br{}, #br{}, #br{}, #br{}, #br{}
    ].
