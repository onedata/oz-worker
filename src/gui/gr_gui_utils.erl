%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2013 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains useful functions commonly used in
%% globalregistry GUI modules.
%% @end
%% ===================================================================

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


%% apply_or_redirect/2
%% ====================================================================
%% @doc Checks if the client has right to do the operation (is logged in). If so, it executes the code.
%% @end
-spec apply_or_redirect(Module :: atom, Fun :: atom) -> boolean().
%% ====================================================================
apply_or_redirect(Module, Fun) ->
    apply_or_redirect(Module, Fun, []).


%% apply_or_redirect/3
%% ====================================================================
%% @doc Checks if the client has right to do the operation (is logged in). If so, it executes the code.
%% @end
-spec apply_or_redirect(Module :: atom, Fun :: atom, Args :: [term()]) -> term() | no_return.
%% ====================================================================
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


%% maybe_redirect/2
%% ====================================================================
%% @doc Decides if user can view the page, depending on arguments.
%% Returns false if no redirection is needed.
%% Otherwise, it issues a redirection and returns true.
%% Setting "SaveSourcePage" on true will allow a redirect back from login.
%% NOTE: Should be called from page:main().
%% @end
-spec maybe_redirect(NeedLogin :: boolean()) -> boolean().
%% ====================================================================
maybe_redirect(NeedLogin) ->
    case NeedLogin and (not gui_ctx:user_logged_in()) of
        true ->
            gui_jq:redirect_to_login(),
            true;
        false ->
            false
    end.


%% get_redirection_url_to_provider/1
%% ====================================================================
%% @doc Returns an URL that the user should be redirected to - if possible.
%% Otherwise, error is returned.
%% If the referer is known (the provider who redirected the user for login),
%% then he will be chosen with highest priority.
%% @end
-spec get_redirection_url_to_provider(Referer :: binary() | undefined) ->
    {ok, ProvderHostname :: binary(), URL :: binary()} | {error, Desc :: no_provider | term()}.
%% ====================================================================
get_redirection_url_to_provider(Referer) ->
    try
        UserID = gui_ctx:get_user_id(),

        % Default provider is the provider that redirected the user for login.
        % Check if the provider is recognisable
        RefererProviderInfo =
            try
                {ProvHostname, RedURL} = auth_logic:get_redirection_uri(UserID, Referer),
                {ok, ProvHostname, RedURL}
            catch _:_ ->
                error
            end,

        case RefererProviderInfo of
            {ok, _, _} ->
                % Default provider is OK
                RefererProviderInfo;
            error ->
                % No default provider, check if default space has any providers
                {ok, [{spaces, Spaces}, {default, DefaultSpace}]} = user_logic:get_spaces(UserID),
                {ok, [{providers, DSProviders}]} = case DefaultSpace of
                                                       undefined -> {ok, [{providers, []}]};
                                                       _ -> space_logic:get_providers(DefaultSpace, user) end,
                case DSProviders of
                    List when length(List) > 0 ->
                        % Default space has got some providers, random one
                        {ProviderHostname, RedirectURL} = auth_logic:get_redirection_uri(
                            UserID, lists:nth(crypto:rand_uniform(1, length(DSProviders) + 1), DSProviders)),
                        {ok, ProviderHostname, RedirectURL};
                    _ ->
                        % Default space does not have a provider, look in other spaces
                        ProviderIDs = lists:foldl(
                            fun(Space, Acc) ->
                                {ok, [{providers, Providers}]} = space_logic:get_providers(Space, user),
                                Providers ++ Acc
                            end, [], Spaces),

                        case ProviderIDs of
                            [] ->
                                % No provider for other spaces = nowhere to redirect
                                {error, no_provider};
                            _ ->
                                % There are some providers for other spaces, redirect to a random provider
                                {ProviderHostname, RedirectURL} = auth_logic:get_redirection_uri(
                                    UserID, lists:nth(crypto:rand_uniform(1, length(ProviderIDs) + 1), ProviderIDs)),
                                {ok, ProviderHostname, RedirectURL}
                        end
                end
        end
    catch T:M ->
        ?error_stacktrace("Cannot resolve redirection URL to provider - ~p:~p", [T, M]),
        {error, no_provider}
    end.


%% top_menu/1
%% ====================================================================
%% @doc Convienience function to render top menu in GUI pages. 
%% Item with ActiveTabID will be highlighted as active.
%% @end
-spec top_menu(ActiveTabID :: term()) -> list().
%% ====================================================================
top_menu(ActiveTabID) ->
    top_menu(ActiveTabID, []).

%% top_menu/2
%% ====================================================================
%% @doc Convienience function to render top menu in GUI pages.
%% Item with ActiveTabID will be highlighted as active.
%% Submenu body (list of n2o elements) will be concatenated below the main menu.
%% @end
-spec top_menu(ActiveTabID :: term(), SubMenuBody :: term()) -> list().
%% ====================================================================
top_menu(ActiveTabID, SubMenuBody) ->
    % Define menu items with ids, so that proper tab can be made active via function parameter
    {ok, #user{name = Name}} = user_logic:get_user(gui_ctx:get_user_id()),

    MenuCaptions = [],

    MenuIcons =
        [
            {manage_account_tab, #li{body = #link{style = <<"padding: 18px;">>, title = <<"Manage account">>,
                url = <<"/manage_account">>, body = [Name, #span{class = <<"fui-user">>,
                    style = <<"margin-left: 10px;">>}]}}},
            %{contact_support_tab, #li { body=#link{ style="padding: 18px;", title="Contact & Support",
            %    url="/contact_support", body=#span{ class="fui-question" } } } },
            {about_tab, #li{body = #link{style = <<"padding: 18px;">>, title = <<"About">>,
                url = <<"/about">>, body = #span{class = <<"fui-info">>}}}},
            {logout_button, #li{body = #link{style = <<"padding: 18px;">>, title = <<"Log out">>,
                url = <<"/logout">>, body = #span{class = <<"fui-power">>}}}}
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

    [
        #panel{class = <<"navbar navbar-fixed-top">>, body = [
            #panel{class = <<"navbar-inner">>, style = <<"border-bottom: 2px solid gray;">>, body = [
                #panel{class = <<"container">>, body = [
                    #list{class = <<"nav pull-left">>, body = MenuCaptionsProcessed},
                    #list{class = <<"nav pull-right">>, body = MenuIconsProcessed}
                ]}
            ]}
        ] ++ SubMenuBody}
    ] ++ gui_utils:cookie_policy_popup_body(<<?privacy_policy_url>>).


%% logotype_footer/1
%% ====================================================================
%% @doc Convienience function to render logotype footer, coming after page content.
%% @end
-spec logotype_footer(MarginTop :: integer()) -> list().
%% ====================================================================
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


% Development functions
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
