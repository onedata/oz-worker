%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains n2o website code.
%% The page displays information about the user and allows some editing.
%% @end
%% ===================================================================

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
-export([redirect_to_veilcluster/3]).


%% Template points to the template file, which will be filled with content
main() ->
    case gr_gui_utils:maybe_redirect(true, true) of
        true ->
            #dtl{file = "bare", app = ?APP_Name, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]};
        false ->
            #dtl{file = "bare", app = ?APP_Name, bindings = [{title, title()}, {body, body()}, {custom, <<"">>}]}
    end.


%% Page title
title() -> <<"Manage account">>.


%% This will be placed in the template instead of [[[page:body()]]] tag
body() ->
    #panel{style = <<"position: relative;">>, body = [
        gr_gui_utils:top_menu(manage_account_tab),
        #panel{style = <<"margin-top: 60px; padding: 20px;">>, body = [
            #h6{style = <<" text-align: center;">>, body = <<"Manage account">>},
            #panel{id = <<"main_table">>, body = main_table()},
            provider_redirection_panel()
        ]}
    ] ++ gr_gui_utils:logotype_footer(20)}.


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
            #span{class = <<"fui-new">>, style = <<"font-size: 16px;">>}},
        #textbox{id = <<"new_name_textbox">>, class = <<"flat">>, body = <<"">>, style = <<"display: none;">>,
            placeholder = <<"New name">>, value = Name},
        #link{id = <<"new_name_submit">>, class = <<"glyph-link">>, style = <<"display: none; margin-left: 10px;">>,
            actions = gui_jq:form_submit_action(<<"new_name_submit">>, {action, update_name}, <<"new_name_textbox">>), body =
            #span{class = <<"fui-check-inverted">>, style = <<"font-size: 20px;">>}},
        #link{id = <<"new_name_cancel">>, class = <<"glyph-link">>, style = <<"display: none; margin-left: 10px;">>,
            postback = {action, show_name_edition, [false]}, body =
            #span{class = <<"fui-cross-inverted">>, style = <<"font-size: 20px;">>}}
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
                        undefined -> <<BasicStyle/binary, "opacity:0.4; filter:alpha(opacity=40);">>;
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
                                                 _ -> <<Acc/binary, "<br />", Mail/binary>>
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
    case gr_gui_utils:get_redirection_url_to_provider() of
        {ok, ProviderHostname, URL} ->
            #panel{body = [
                #button{body = <<"Go to your files">>, class = <<"btn btn-huge btn-inverse btn-block">>,
                    postback = {action, redirect_to_veilcluster, [ProviderHostname, URL, true]}}
            ]};
        {error, no_provider} ->
            case gui_ctx:get(referer) of
                undefined ->
                    {ok, #user{first_space_support_token = Token}} = user_logic:get_user(gui_ctx:get_user_id()),
                    gui_jq:select_text(<<"token_textbox">>),
                    #panel{class = <<"dialog dialog-danger">>, body = [
                        #p{body = <<"Currently, none of your spaces are supported by any provider. To access your files, ",
                        "you must find a provider willing to support your space. Below is a token that you should give to the provider:">>},
                        #textbox{id = <<"token_textbox">>, class = <<"flat">>, style = <<"width: 500px;">>,
                            value = Token, placeholder = <<"Space support token">>}
                    ]};
                Referer ->
                    #panel{body = [
                        #button{body = <<"Go to your files">>, class = <<"btn btn-huge btn-inverse btn-block">>,
                            postback = {action, redirect_to_veilcluster, [Referer, Referer, true]}}
                    ]};
            end;
        _ ->
            page_error:redirect_with_error(?error_internal_server_error)
end.


% Postback event handling
event(init) -> ok;

event({action, Fun}) ->
    event({action, Fun, []});

event({action, Fun, Args}) ->
    gr_gui_utils:apply_or_redirect(?MODULE, Fun, Args).


% Connects an oauth account to users account
connect_account(Provider) ->
    HandlerModule = auth_config:get_provider_module(Provider),
    {ok, URL} = HandlerModule:get_redirect_url(true),
    gui_jq:redirect(URL).


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
            NewEmail = gui_utils:normalize_email(gui_ctx:postback_param(<<"new_email_textbox">>)),
            case user_logic:get_user({email, NewEmail}) of
                {ok, _} ->
                    gui_jq:wire(#alert{text = <<"This e-mail address is in use.">>});
                _ ->
                    case gui_utils:validate_email(NewEmail) of
                        false ->
                            gui_jq:wire(#alert{text = <<"Please enter a valid email address.">>});
                        true ->
                            case user_logic:modify(UserId, [{email_list, OldEmailList ++ [NewEmail]}]) of
                                ok -> ok;
                                _ -> gui_jq:wire(#alert{text = <<"Error - cannot update email list.">>})
                            end
                    end
            end;
        {remove, Email} ->
            case user_logic:modify(UserId, [{email_list, OldEmailList -- [Email]}]) of
                ok -> ok;
                _ -> gui_jq:wire(#alert{text = <<"Error - cannot update email list.">>})
            end
    end,
    gui_jq:update(<<"main_table">>, main_table()).


% Update email list - add or remove one and save new user doc
update_name() ->
    GlobalID = gui_ctx:get_user_id(),
    NewName = gui_ctx:postback_param(<<"new_name_textbox">>),
    case user_logic:modify(GlobalID, [{name, NewName}]) of
        ok -> ok;
        _ -> gui_jq:wire(#alert{text = <<"Error - cannot update name.">>})
    end,
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


% Show email adding form
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


redirect_to_veilcluster(ProviderHostname, URL, CheckConnectivity) ->
    case {CheckConnectivity, gui_utils:https_get(<<ProviderHostname/binary, ?veilcluster_connection_check_endpoint>>, [])} of
        {true, {ok, _}} ->
            gui_jq:redirect(URL);
        _ ->
            gui_jq:wire(#alert{text = <<"The provider that supports your space(s) is currently unreachable.">>})
    end.
