%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc: This file contains n2o website code.
%%% The page displays information about the user and allows some editing.
%%% @end
%%%-------------------------------------------------------------------

-module(page_manage_account).

-include_lib("ctool/include/logging.hrl").
-include("dao/dao_types.hrl").
-include("auth_common.hrl").
-include("gui/common.hrl").

% n2o API
-export([main/0, event/1]).
% Postback functions and other
-export([connect_account/1, disconnect_account_prompt/1, disconnect_account/1]).
-export([show_email_adding/1, update_email/1, show_name_edition/1, update_name/0]).
-export([show_alias_edition/1, update_alias/0, redirect_to_provider/2, show_alias_info/0]).
-export([redirect_to_provider_dev/1, generate_token/0]).

%% Template points to the template file, which will be filled with content
main() ->
    case gr_gui_utils:maybe_redirect(true) of
        true ->
            #dtl{file = "bare", app = ?APP_Name, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]};
        false ->
            #dtl{file = "bare", app = ?APP_Name, bindings = [{title, title()}, {body, body()}, {custom, <<"">>}]}
    end.

%% Page title
title() -> <<"Manage account">>.

%% This will be placed in the template instead of [[[page:body()]]] tag
body() ->
    #panel{class = <<"page-container">>, body = [
        gr_gui_utils:top_menu(manage_account_tab),
        #panel{style = <<"margin-top: 60px; padding: 20px;">>, body = [
            #h6{style = <<" text-align: center;">>, body = <<"Manage account">>},
            #panel{id = <<"main_table">>, body = main_table()},
            #panel{id = <<"gen_token_panel">>, style = <<"margin-top: 10px; margin-bottom: 25px;">>, body = [
                #button{body = <<"Generate client token">>,
                    class = <<"btn btn-inverse">>,
                    postback = {action, generate_token}}
            ]},
            provider_redirection_panel()
        ]}
    ]}.

%% Main table containing user account info
main_table() ->
    {ok, #user{} = User} = user_logic:get_user(gui_ctx:get_user_id()),
    #table{style = <<"border-width: 0px; width: auto;">>, body = [
        #tbody{body = [
            #tr{cells = [
                #td{style = <<"padding: 15px; vertical-align: top;">>, body =
                #label{class = <<"label label-large label-inverse">>, style = <<"cursor: auto;">>, body = <<"Name">>}},
                #td{style = <<"padding: 15px; vertical-align: top;">>, body = user_name_section(User)}
            ]},

            #tr{cells = [
                #td{style = <<"padding: 15px; vertical-align: top;">>, body =
                #label{class = <<"label label-large label-inverse">>, style = <<"cursor: auto;">>, body = <<"Alias">>}},
                #td{style = <<"padding: 15px; vertical-align: top;">>, body = alias_section(User)}
            ]},

            #tr{cells = [
                #td{style = <<"padding: 15px; vertical-align: top;">>, body =
                #label{class = <<"label label-large label-inverse">>, style = <<"cursor: auto;">>, body = <<"E-mails">>}},
                #td{style = <<"padding: 15px; vertical-align: top;">>, body = user_emails_section(User)}
            ]},

            #tr{cells = [
                #td{style = <<"padding: 15px; vertical-align: top;">>, body =
                #label{class = <<"label label-large label-inverse">>, style = <<"cursor: auto;">>, body = <<"Connected<br />accounts">>}},
                #td{style = <<"padding: 15px; vertical-align: top;">>, body = connected_accounts_section(User)}
            ]}
        ]}
    ]}.

%% Table row with user name edition
user_name_section(User) ->
    gui_jq:bind_enter_to_submit_button(<<"new_name_textbox">>, <<"new_name_submit">>),
    #user{name = Name} = User,
    [
        #span{style = <<"font-size: 18px;">>, id = <<"displayed_name">>, body = Name},
        #link{id = <<"change_name_button">>, class = <<"glyph-link">>, style = <<"margin-left: 10px;">>,
            postback = {action, show_name_edition, [true]}, body =
            #span{class = <<"icomoon-pencil2">>, style = <<"font-size: 16px;">>}},
        #textbox{id = <<"new_name_textbox">>, class = <<"flat">>, body = <<"">>, style = <<"display: none;">>,
            placeholder = <<"New name">>, value = Name},
        #link{id = <<"new_name_submit">>, class = <<"glyph-link">>, style = <<"display: none; margin-left: 10px;">>,
            actions = gui_jq:form_submit_action(<<"new_name_submit">>, {action, update_name}, <<"new_name_textbox">>), body =
            #span{class = <<"fui-check-inverted">>, style = <<"font-size: 20px;">>}},
        #link{id = <<"new_name_cancel">>, class = <<"glyph-link">>, style = <<"display: none; margin-left: 10px;">>,
            postback = {action, show_name_edition, [false]}, body =
            #span{class = <<"fui-cross-inverted">>, style = <<"font-size: 20px;">>}}
    ].

%% Table row with user alias edition
alias_section(User) ->
    gui_jq:bind_enter_to_submit_button(<<"new_alias_textbox">>, <<"new_alias_submit">>),
    #user{alias = Alias} = User,
    {AliasStyle, AliasBody, AliasHint} =
        case Alias of
            ?EMPTY_ALIAS ->
                {<<"font-size: 18px; color: lightgray;">>, <<"no alias">>, <<"">>};
            _ ->
                {<<"font-size: 18px;">>, Alias, Alias}
        end,
    [
        #span{style = AliasStyle, id = <<"displayed_alias">>, body = AliasBody},
        #link{id = <<"change_alias_button">>, class = <<"glyph-link">>, style = <<"margin-left: 10px;">>,
            postback = {action, show_alias_edition, [true]}, body =
            #span{class = <<"icomoon-pencil2">>, style = <<"font-size: 16px;">>}},
        #textbox{id = <<"new_alias_textbox">>, class = <<"flat">>, body = <<"">>, style = <<"display: none;">>,
            placeholder = <<"New alias">>, value = AliasHint},
        #link{id = <<"new_alias_submit">>, class = <<"glyph-link">>, style = <<"display: none; margin-left: 10px;">>,
            actions = gui_jq:form_submit_action(<<"new_alias_submit">>, {action, update_alias}, <<"new_alias_textbox">>), body =
            #span{class = <<"fui-check-inverted">>, style = <<"font-size: 20px;">>}},
        #link{id = <<"new_alias_cancel">>, class = <<"glyph-link">>, style = <<"display: none; margin-left: 10px;">>,
            postback = {action, show_alias_edition, [false]}, body =
            #span{class = <<"fui-cross-inverted">>, style = <<"font-size: 20px;">>}},
        #link{id = <<"alias_info">>, class = <<"glyph-link">>, style = <<"margin-left: 30px;">>,
            postback = {action, show_alias_info}, body = #span{class = <<"icomoon-help">>, style = <<"font-size: 16px;">>}}
    ].

% HTML list with emails printed
user_emails_section(User) ->
    #user{email_list = Emails} = User,

    {CurrentEmails, _} = lists:mapfoldl(
        fun(Email, Acc) ->
            Body = #li{style = <<"font-size: 18px; padding: 5px 0;">>, body = #span{body =
            [
                Email,
                #link{id = <<"remove_email_button", (integer_to_binary(Acc))/binary>>, class = <<"glyph-link">>, style = <<"margin-left: 10px;">>,
                    postback = {action, update_email, [{remove, Email}]}, body =
                    #span{class = <<"fui-cross">>, style = <<"font-size: 16px;">>}}
            ]}},
            {Body, Acc + 1}
        end, 1, Emails),
    NewEmail = [
        #li{style = <<"font-size: 18px; padding: 5px 0;">>, body = [
            #link{id = <<"add_email_button">>, class = <<"glyph-link">>, style = <<"margin-left: 10px;">>,
                postback = {action, show_email_adding, [true]}, body =
                #span{class = <<"fui-plus">>, style = <<"font-size: 16px; position: relative;">>}},
            #textbox{id = <<"new_email_textbox">>, class = <<"flat">>, body = <<"">>, style = <<"display: none;">>,
                placeholder = <<"New email address">>},
            #link{id = <<"new_email_submit">>, class = <<"glyph-link">>, style = <<"display: none; margin-left: 10px;">>,
                actions = gui_jq:form_submit_action(<<"new_email_submit">>, {action, update_email, [{add, submitted}]}, <<"new_email_textbox">>), body =
                #span{class = <<"fui-check-inverted">>, style = <<"font-size: 20px;">>}},
            #link{id = <<"new_email_cancel">>, class = <<"glyph-link">>, style = <<"display: none; margin-left: 10px;">>,
                postback = {action, show_email_adding, [false]}, body =
                #span{class = <<"fui-cross-inverted">>, style = <<"font-size: 20px;">>}}
        ]}
    ],
    gui_jq:bind_enter_to_submit_button(<<"new_email_textbox">>, <<"new_email_submit">>),
    #list{numbered = true, style = <<"margin-top: -3px;">>, body = CurrentEmails ++ NewEmail}.

% Section allowing for edition of connected accounts
connected_accounts_section(User) ->
    #user{connected_accounts = ConnectedAccounts} = User,
    TableHead = #tr{cells = [
        #th{body = <<"">>},
        #th{body = <<"provider">>},
        #th{body = <<"e-mails">>},
        #th{body = <<"login">>},
        #th{body = <<"name">>}
    ]},
    TableBody = lists:map(
        fun(Provider) ->
            ProviderInfo = find_connected_account(Provider, ConnectedAccounts),
            ProviderName = auth_config:get_provider_name(Provider),

            % Checkbox
            {CheckboxIcon, CheckboxTitle, CheckboxStyle, CheckboxPostback} =
                case ProviderInfo of
                    undefined ->
                        {<<"fui-checkbox-unchecked">>,
                            <<"Connect ", ProviderName/binary, " account">>,
                            <<"line-height: 32px; font-size: 25px; color: rgb(200,200,200);">>,
                            {action, connect_account, [Provider]}};
                    _ ->
                        {<<"fui-checkbox-checked">>,
                            <<"Disonnect ", ProviderName/binary, " account">>,
                            <<"line-height: 32px; font-size: 25px; color: #1ABC9C;">>,
                            {action, disconnect_account_prompt, [Provider]}}
                end,

            Checkbox = #link{title = CheckboxTitle, style = CheckboxStyle, postback = CheckboxPostback, body = #span{class = CheckboxIcon}},

            % Provider label
            Icon = auth_config:get_provider_button_icon(Provider),
            Color = auth_config:get_provider_button_color(Provider),
            BasicStyle = <<"margin: 0; line-height: 32px; cursor: auto; padding: 5px; text-align: left; width: 130px; ",
            "color: white; background-color: ", Color/binary, ";">>,
            Style = case ProviderInfo of
                        undefined ->
                            <<BasicStyle/binary, "opacity:0.4; filter:alpha(opacity=40);">>;
                        _ -> BasicStyle
                    end,
            ProviderLabel = #label{class = <<"label">>, style = Style, body = [
                #image{image = Icon, style = <<"margin-right: 10px;">>},
                ProviderName
            ]},

            % Emails
            Emails = case ProviderInfo of
                         undefined ->
                             <<"">>;
                         #oauth_account{email_list = List} ->
                             case List of
                                 [] ->
                                     <<"-">>;
                                 _ ->
                                     lists:foldl(
                                         fun(Mail, Acc) ->
                                             case Acc of
                                                 <<"">> -> Mail;
                                                 _ ->
                                                     <<Acc/binary, "<br />", Mail/binary>>
                                             end
                                         end, <<"">>, List)
                             end
                     end,

            % Login
            Login = case ProviderInfo of
                        undefined ->
                            <<"">>;
                        #oauth_account{login = SomeLogin} ->
                            case SomeLogin of
                                <<"">> -> <<"-">>;
                                _ -> SomeLogin
                            end
                    end,

            % Name
            Name = case ProviderInfo of
                       undefined ->
                           <<"">>;
                       #oauth_account{name = SomeName} ->
                           case SomeName of
                               <<"">> -> <<"-">>;
                               _ -> SomeName
                           end
                   end,

            #tr{cells = [
                #td{style = <<"vertical-align: middle;">>, body = Checkbox},
                #td{body = ProviderLabel},
                #td{style = <<"vertical-align: middle;">>, body = Emails},
                #td{style = <<"vertical-align: middle;">>, body = Login},
                #td{style = <<"vertical-align: middle;">>, body = Name}
            ]}
        end, auth_config:get_auth_providers()),

    Table = #table{class = <<"table table-bordered">>, body = [
        #tbody{body = [TableHead | TableBody]}
    ]},
    #panel{style = <<"position: relative;">>, body = Table}.

% Finds a connected accounr record in list of records
find_connected_account(Provider, ProviderInfos) ->
    lists:foldl(fun(ProvUserInfo = #oauth_account{provider_id = ProviderID}, Acc) ->
        case Provider of
            ProviderID -> ProvUserInfo;
            _ -> Acc
        end
    end, undefined, ProviderInfos).

% Panel that will display a button to redirect a user to his provider,
% or a token for space support if he has no spaces supported.
provider_redirection_panel() ->
    % If dev_mode is on, allow to log in on any provider.
    case application:get_env(?APP_Name, dev_mode) of
        {ok, true} ->
            UserID = gui_ctx:get_user_id(),
            {ok, Props} = user_logic:get_spaces(UserID),
            Spaces = proplists:get_value(spaces, Props),
            Providers = lists:foldl(
                fun(Space, Acc) ->
                    {ok, [{providers, Providers}]} = space_logic:get_providers(Space, provider),
                    Providers ++ Acc
                end, [], Spaces),
            #panel{id = <<"redirection_panel">>, body = lists:map(
                fun(ProviderID) ->
                    #button{body = <<"Go to your files in provider <b>", ProviderID/binary, "</b>">>,
                        class = <<"btn btn-huge btn-inverse">>,
                        postback = {action, redirect_to_provider_dev, [ProviderID]}}
                end, lists:usort(Providers))
            };
        _ ->
            case gr_gui_utils:get_redirection_url_to_provider(gui_ctx:get(referer)) of
                {ok, ProviderHostname, URL} ->
                    #panel{id = <<"redirection_panel">>, body = [
                        #button{body = <<"Go to your files">>, class = <<"btn btn-huge btn-inverse">>,
                            postback = {action, redirect_to_provider, [ProviderHostname, URL]}}
                    ]};
                {error, no_provider} ->
                    {ok, #user{first_space_support_token = Token}} = user_logic:get_user(gui_ctx:get_user_id()),
                    gui_jq:select_text(<<"token_textbox">>),
                    #panel{id = <<"redirection_panel">>, class = <<"dialog dialog-danger">>, body = [
                        #p{body = <<"Currently, none of your spaces are supported by any provider. To access your files, ",
                        "you must find a provider willing to support your space. Below is a token that you should give to the provider:">>},
                        #textbox{id = <<"token_textbox">>, class = <<"flat">>, style = <<"width: 500px;">>,
                            value = Token, placeholder = <<"Space support token">>},
                        #p{style = <<"margin-top: 40px;">>, body = <<"You can also become a provider yourself and support your own space:">>},
                        #link{class = <<"btn btn-success">>, url = <<?become_a_provider_url>>,
                            style = <<"width: 300px;">>, body = <<"Read more">>}
                    ]}
            end
    end.

%%--------------------------------------------------------------------
%% @doc Postback event handling
%% @end
%%--------------------------------------------------------------------
-spec event(init | {action, Fun :: atom()} | {action, Fun :: atom(), Args :: [term()]}) -> term().
event(init) ->
    ok;

event({action, Fun}) ->
    event({action, Fun, []});

event({action, Fun, Args}) ->
    gr_gui_utils:apply_or_redirect(?MODULE, Fun, Args).

% Connects an oauth account to users account
connect_account(Provider) ->
    HandlerModule = auth_config:get_provider_module(Provider),
    {ok, URL} = HandlerModule:get_redirect_url(true),
    gui_jq:redirect(URL).


generate_token() ->
    UserID = gui_ctx:get_user_id(),
    Token = auth_logic:gen_token(UserID),
    gui_jq:info_popup(<<"Client token">>, Token, <<"">>).


% Prompt to ask for confirmation to disconnect an account
disconnect_account_prompt(Provider) ->
    % Get user info doc
    {ok, #user{connected_accounts = ConnectedAccounts}} = user_logic:get_user(gui_ctx:get_user_id()),
    % Get provider name
    ProviderName = auth_config:get_provider_name(Provider),
    case length(ConnectedAccounts) of
        1 ->
            % Prevent from disconnecting last account
            gui_jq:wire(#alert{text = <<"You cannot disconnect your last account.">>});
        _ ->
            % Prompt for confirmation to delete
            gui_jq:wire(#confirm{text = <<"Are you sure you want to disconnect your ", ProviderName/binary, " account?">>,
                postback = {action, disconnect_account, [Provider]}})
    end.

% Disconnects an oauth account from user's accounts
disconnect_account(Provider) ->
    UserId = gui_ctx:get_user_id(),
    % Find the user, remove provider info from his user info doc and reload the page
    {ok, #user{connected_accounts = ConnectedAccounts}} = user_logic:get_user(UserId),
    OAuthAccount = find_connected_account(Provider, ConnectedAccounts),
    user_logic:modify(UserId, [{connected_accounts, ConnectedAccounts -- [OAuthAccount]}]),
    gui_jq:redirect(<<"/manage_account">>).

% Update email list - add or remove one and save new user doc
update_email(AddOrRemove) ->
    UserId = gui_ctx:get_user_id(),
    {ok, #user{email_list = OldEmailList}} = user_logic:get_user(UserId),
    case AddOrRemove of
        {add, submitted} ->
            NewEmail = http_utils:normalize_email(gui_ctx:postback_param(<<"new_email_textbox">>)),
            case user_logic:get_user({email, NewEmail}) of
                {ok, _} ->
                    gui_jq:wire(#alert{text = <<"This e-mail address is in use.">>});
                _ ->
                    case http_utils:validate_email(NewEmail) of
                        false ->
                            gui_jq:wire(#alert{text = <<"Please enter a valid email address.">>});
                        true ->
                            case user_logic:modify(UserId, [{email_list, OldEmailList ++ [NewEmail]}]) of
                                ok -> ok;
                                _ ->
                                    gui_jq:wire(#alert{text = <<"Error - cannot update email list.">>})
                            end
                    end
            end;
        {remove, Email} ->
            case user_logic:modify(UserId, [{email_list, OldEmailList -- [Email]}]) of
                ok -> ok;
                _ ->
                    gui_jq:wire(#alert{text = <<"Error - cannot update email list.">>})
            end
    end,
    gui_jq:update(<<"main_table">>, main_table()).

% Update user name
update_name() ->
    GlobalID = gui_ctx:get_user_id(),
    NewName = gui_ctx:postback_param(<<"new_name_textbox">>),
    case user_logic:modify(GlobalID, [{name, NewName}]) of
        ok -> ok;
        _ -> gui_jq:wire(#alert{text = <<"Error - cannot update name.">>})
    end,
    gui_jq:update(<<"main_table">>, main_table()).

% Update user alias
update_alias() ->
    GlobalID = gui_ctx:get_user_id(),
    NewAlias = case gui_ctx:postback_param(<<"new_alias_textbox">>) of
                   <<"">> -> ?EMPTY_ALIAS;
                   Bin -> Bin
               end,
    case user_logic:modify(GlobalID, [{alias, NewAlias}]) of
        ok ->
            ok;
        {error, disallowed_prefix} ->
            gui_jq:info_popup(<<"Error - cannot update alias">>, <<"Alias cannot start with \"", ?NO_ALIAS_UUID_PREFIX, "\".">>, <<"">>);
        {error, invalid_alias} ->
            gui_jq:info_popup(<<"Error - cannot update alias">>, <<"Alias can contain only lowercase letters and digits, and must be at least 5 characters long.">>, <<"">>);
        {error, alias_occupied} ->
            gui_jq:info_popup(<<"Error - cannot update alias">>, <<"This alias is occupied by someone else. Please choose other alias.">>, <<"">>);
        {error, alias_conflict} ->
            gui_jq:info_popup(<<"Error - cannot update alias">>, <<"This alias is occupied by someone else. Please choose other alias.">>, <<"">>);
        _ ->
            gui_jq:info_popup(<<"Error - cannot update alias">>, <<"Please try again later.">>, <<"">>)
    end,
    gui_jq:replace(<<"redirection_panel">>, provider_redirection_panel()),
    gui_jq:update(<<"main_table">>, main_table()).

% Show email adding form
show_email_adding(Flag) ->
    case Flag of
        true ->
            gui_jq:hide(<<"add_email_button">>),
            gui_jq:fade_in(<<"new_email_textbox">>, 300),
            gui_jq:fade_in(<<"new_email_cancel">>, 300),
            gui_jq:fade_in(<<"new_email_submit">>, 300),
            gui_jq:focus(<<"new_email_textbox">>);
        false ->
            gui_jq:fade_in(<<"add_email_button">>, 300),
            gui_jq:hide(<<"new_email_textbox">>),
            gui_jq:hide(<<"new_email_cancel">>),
            gui_jq:hide(<<"new_email_submit">>)
    end.

% Show name adding form
show_name_edition(Flag) ->
    case Flag of
        true ->
            gui_jq:hide(<<"displayed_name">>),
            gui_jq:hide(<<"change_name_button">>),
            gui_jq:fade_in(<<"new_name_textbox">>, 300),
            gui_jq:fade_in(<<"new_name_cancel">>, 300),
            gui_jq:fade_in(<<"new_name_submit">>, 300),
            gui_jq:focus(<<"new_name_textbox">>),
            gui_jq:select_text(<<"new_name_textbox">>);
        false ->
            gui_jq:fade_in(<<"displayed_name">>, 300),
            gui_jq:fade_in(<<"change_name_button">>, 300),
            gui_jq:hide(<<"new_name_textbox">>),
            gui_jq:hide(<<"new_name_cancel">>),
            gui_jq:hide(<<"new_name_submit">>)
    end.

% Show alias adding form
show_alias_edition(Flag) ->
    case Flag of
        true ->
            gui_jq:hide(<<"displayed_alias">>),
            gui_jq:hide(<<"change_alias_button">>),
            gui_jq:fade_in(<<"new_alias_textbox">>, 300),
            gui_jq:fade_in(<<"new_alias_cancel">>, 300),
            gui_jq:fade_in(<<"new_alias_submit">>, 300),
            gui_jq:focus(<<"new_alias_textbox">>),
            gui_jq:select_text(<<"new_alias_textbox">>);
        false ->
            gui_jq:fade_in(<<"displayed_alias">>, 300),
            gui_jq:fade_in(<<"change_alias_button">>, 300),
            gui_jq:hide(<<"new_alias_textbox">>),
            gui_jq:hide(<<"new_alias_cancel">>),
            gui_jq:hide(<<"new_alias_submit">>)
    end.


redirect_to_provider(ProviderHostname, URL) ->
    case gui_utils:https_get(<<ProviderHostname/binary, ?provider_connection_check_endpoint>>, []) of
        {ok, _} ->
            gui_jq:redirect(URL);
        _ ->
            gui_jq:wire(#alert{text = <<"The provider that supports your space(s) is currently unreachable. Try again later.">>})
    end.


redirect_to_provider_dev(ProviderID) ->
    {ok, ProviderHostname, URL} = gr_gui_utils:get_redirection_url_to_provider(ProviderID),
    case gui_utils:https_get(<<ProviderHostname/binary, ?provider_connection_check_endpoint>>, []) of
        {ok, _} ->
            gui_jq:redirect(URL);
        _ ->
            gui_jq:wire(#alert{text = <<"The provider that supports your space(s) is currently unreachable. Try again later.">>})
    end.

% Show info about aliases
show_alias_info() ->
    gui_jq:info_popup(
        <<"Alias info">>,
        <<"Alias is a unique identifier of user&#39;s choice (it can be perceived as a login). ",
        "It allows for easy recognition of users and can be changed any time. Having an alias is not mandatory.">>,
        <<"">>
    ).