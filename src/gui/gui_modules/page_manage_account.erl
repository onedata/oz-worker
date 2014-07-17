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
-export([redirect_to_veilcluster/0]).

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
            #button{body = <<"Go to your files - UWAGA TO NIE DZIALA">>, class = <<"btn btn-huge btn-inverse btn-block">>, postback = {action, redirect_to_veilcluster}}
        ]}
    ] ++ gr_gui_utils:logotype_footer(20)}.


main_table() ->
    {ok, #user{} = User} = user_logic:get_user(gui_ctx:get_user_id()),
    #table{style = <<"border-width: 0px; width: auto;">>, body = [
        #tbody{body = [
            #tr{cells = [
                #td{style = <<"border-width: 0px; padding: 10px 10px; vertical-align: top;">>, body =
                #label{class = <<"label label-large label-inverse">>, style = <<"cursor: auto;">>, body = <<"Name">>}},
                #td{style = <<"border-width: 0px; padding: 10px 10px">>, body = user_name_section(User)}
            ]},

            #tr{cells = [
                #td{style = <<"border-width: 0px; padding: 10px 10px; vertical-align: top;">>, body =
                #label{class = <<"label label-large label-inverse">>, style = <<"cursor: auto;">>, body = <<"E-mails">>}},
                #td{style = <<"border-width: 0px; padding: 10px 10px">>, body = user_emails_section(User)}
            ]},

            #tr{cells = [
                #td{style = <<"border-width: 0px; padding: 10px 10px;  vertical-align: top;">>, body =
                #label{class = <<"label label-large label-inverse">>, style = <<"cursor: auto;">>, body = <<"Connected<br />accounts">>}},
                #td{style = <<"border-width: 0px; padding: 10px 10px">>, body = connected_accounts_section(User)}
            ]}
        ]}
    ]}.


user_name_section(User) ->
    #user{name = Name} = User,
    [
        #span{style = <<"font-size: 18px;">>, id = <<"displayed_name">>, body = Name},
        #link{id = <<"change_name_button">>, class = <<"glyph-link">>, style = <<"margin-left: 10px;">>,
            postback = {action, show_name_edition, [true]}, body =
            #span{class = <<"fui-new">>, style = <<"font-size: 16px;">>}},
        #textbox{id = <<"new_name_textbox">>, class = <<"flat">>, body = <<"">>, style = <<"display: none;">>,
            placeholder = <<"New name">>, postback = {action, update_name},
            source = ["new_name_textbox"]},
        #link{id = <<"new_name_submit">>, class = <<"glyph-link">>, style = <<"display: none; margin-left: 10px;">>,
            postback = {action, update_name}, source = ["new_name_textbox"], body =
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
                placeholder = <<"New email address">>, postback = {action, update_email, [{add, submitted}]},
                source = ["new_email_textbox"]},
            #link{id = <<"new_email_submit">>, class = <<"glyph-link">>, style = <<"display: none; margin-left: 10px;">>,
                postback = {action, update_email, [{add, submitted}]}, source = ["new_email_textbox"], body =
                #span{class = <<"fui-check-inverted">>, style = <<"font-size: 20px;">>}},
            #link{id = <<"new_email_cancel">>, class = <<"glyph-link">>, style = <<"display: none; margin-left: 10px;">>,
                postback = {action, show_email_adding, [false]}, body =
                #span{class = <<"fui-cross-inverted">>, style = <<"font-size: 20px;">>}}
        ]}
    ],
    #list{numbered = true, body = CurrentEmails ++ NewEmail}.


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


find_connected_account(Provider, ProviderInfos) ->
    lists:foldl(fun(ProvUserInfo = #oauth_account{provider_id = ProviderID}, Acc) ->
        case Provider of
            ProviderID -> ProvUserInfo;
            _ -> Acc
        end
    end, undefined, ProviderInfos).


% Postback event handling
event(init) -> ok;

event({action, Fun}) ->
    event({action, Fun, []});

event({action, Fun, Args}) ->
    gr_gui_utils:apply_or_redirect(?MODULE, Fun, Args, false).


connect_account(Provider) ->
    HandlerModule = auth_config:get_provider_module(Provider),
    {ok, URL} = HandlerModule:get_redirect_url(true),
    gui_jq:redirect(URL).


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
            NewEmail = auth_utils:normalize_email(gui_ctx:postback_param("new_email_textbox")),
            case user_logic:get_user({email, NewEmail}) of
                undefined ->
                    case user_logic:modify(UserId, [{email_list, OldEmailList ++ [NewEmail]}]) of
                        ok -> ok;
                        _ -> gui_jq:wire(#alert{text = <<"Error - cannot update email list.">>})
                    end;
                _ ->
                    gui_jq:wire(#alert{text = <<"This e-mail address is in use.">>})
            end;
        {remove, Email} ->
            case user_logic:modify(UserId, [{email_list, OldEmailList -- [Email]}]) of
                ok -> ok;
                _ -> gui_jq:wire(#alert{text = <<"Error - cannot update email list.">>})
            end
    end,
    gui_jq:update("main_table", main_table()).


% Update email list - add or remove one and save new user doc
update_name() ->
    GlobalID = gui_ctx:get_user_id(),
    NewName = gui_ctx:postback_param("new_name_textbox"),
    case user_logic:modify(GlobalID, [{name, NewName}]) of
        ok -> ok;
        _ -> gui_jq:wire(#alert{text = <<"Error - cannot update name.">>})
    end,
    gui_jq:update("main_table", main_table()).


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
            gui_jq:focus(<<"new_name_textbox">>);
        false ->
            gui_jq:fade_in(<<"displayed_name">>, 300),
            gui_jq:fade_in(<<"change_name_button">>, 300),
            gui_jq:hide(<<"new_name_textbox">>),
            gui_jq:hide(<<"new_name_cancel">>),
            gui_jq:hide(<<"new_name_submit">>)
    end.


redirect_to_veilcluster() ->
    UserID = gui_ctx:get_user_id(),
%%     RedirectURL = onedata_auth:get_redirect_to_provider_url(<<"https://veilfsdev.com">>, UserID),\
    try
        RedirectURL = auth_logic:get_redirection_uri(UserID, <<"04fa9242bcdc9cb0a1bfe72d3df9054d">>),
        gui_jq:redirect(RedirectURL)
    catch T:M ->
        ?error_stacktrace("tutut ~p:~p", [T, M])
    end.
%%     <<"veilfsdev.com/openid_login?authorization_code=", Rest/binary>> = _RedirectURL,
%%     gui_jq:redirect(<<"https://onedata.org/auth_endpoint?authorization_code=", Rest/binary>>).