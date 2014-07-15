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
-compile(export_all).

-include("dao/dao_types.hrl").
-include("auth_common.hrl").
-include("gui_common.hrl").
-include("logging.hrl").

%% Template points to the template file, which will be filled with content
main() ->
    case gui_utils:maybe_redirect(true, false, false, true) of
        true ->
            #dtl{file = "bare", app = veil_cluster_node, bindings = [{title, <<"">>}, {body, <<"">>}]};
        false ->
            #dtl{file = "bare", app = veil_cluster_node, bindings = [{title, title()}, {body, body()}]}
    end.

%% Page title
title() -> <<"Manage account">>.

%% This will be placed in the template instead of [[[page:body()]]] tag
body() ->
    #panel{style = <<"position: relative;">>, body = [
        gui_utils:top_menu(manage_account_tab),
        #panel{style = <<"margin-top: 60px; padding: 20px;">>, body = [
            #h6{style = <<" text-align: center;">>, body = <<"Manage account">>},
            #panel{id = <<"main_table">>, body = main_table()},
            #button{body = <<"Go to your files - UWAGA TO NIE DZIALA">>, class = <<"btn btn-huge btn-inverse btn-block">>, postback = {action, redirect_to_veilcluster}}
        ]}
    ] ++ gui_utils:logotype_footer(20)}.


main_table() ->
    User = get_user_record(wf:user()),
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
            ProviderName = auth_utils:get_provider_name(Provider),

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
            Icon = auth_utils:get_provider_button_icon(Provider),
            Color = auth_utils:get_provider_button_color(Provider),
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
        end, auth_utils:get_auth_providers()),

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
    gui_utils:apply_or_redirect(?MODULE, Fun, Args, false).


connect_account(Provider) ->
    HandlerModule = auth_utils:get_provider_module(Provider),
    {ok, URL} = HandlerModule:get_redirect_url(true),
    wf:redirect(URL).


disconnect_account_prompt(Provider) ->
    % Get user info doc
    #user{connected_accounts = ConnectedAccounts} = get_user_record(wf:user()),
    % Get provider name
    ProviderName = auth_utils:get_provider_name(Provider),
    case length(ConnectedAccounts) of
        1 ->
            % Prevent from disconnecting last account
            wf:wire(#alert{text = <<"You cannot disconnect your last account.">>});
        _ ->
            % Prompt for confirmation to delete
            wf:wire(#confirm{text = <<"Are you sure you want to disconnect your ", ProviderName/binary, " account?">>,
                postback = {action, disconnect_account, [Provider]}})
    end.


disconnect_account(Provider) ->
    UserId = wf:user(),
    % Find the user, remove provider info from his user info doc and reload the page
    #user{connected_accounts = ConnectedAccounts} = get_user_record(UserId),
    OAuthAccount = find_connected_account(Provider, ConnectedAccounts),
    user_logic:modify(UserId, [{connected_accounts, ConnectedAccounts -- [OAuthAccount]}]),
    wf:redirect(<<"/manage_account">>).


% Update email list - add or remove one and save new user doc
update_email(AddOrRemove) ->
    UserId = wf:user(),
    #user{email_list = OldEmailList} = get_user_record(UserId),
    case AddOrRemove of
        {add, submitted} ->
            NewEmail = auth_utils:normalize_email(gui_utils:to_binary(wf:q("new_email_textbox"))),
            case get_user_record({email, NewEmail}) of
                undefined ->
                    user_logic:modify(UserId, [{email_list, OldEmailList ++ [NewEmail]}]);
                _ ->
                    wf:wire(#alert{text = <<"This e-mail address is in use.">>})
            end;
        {remove, Email} ->
            user_logic:modify(UserId, [{email_list, OldEmailList -- [Email]}])
    end,
    gui_utils:update("main_table", main_table()).


% Update email list - add or remove one and save new user doc
update_name() ->
    GlobalID = wf:user(),
    NewName = gui_utils:to_binary(wf:q("new_name_textbox")),
    user_logic:modify(GlobalID, [{name, NewName}]),
    gui_utils:update("main_table", main_table()).


% Show email adding form
show_email_adding(Flag) ->
    case Flag of
        true ->
            wf:wire(#jquery{target = "add_email_button", method = ["hide"]}),
            wf:wire(#jquery{target = "new_email_textbox", method = ["fadeIn"], args = [300]}),
            wf:wire(#jquery{target = "new_email_cancel", method = ["fadeIn"], args = [300]}),
            wf:wire(#jquery{target = "new_email_submit", method = ["fadeIn"], args = [300]}),
            wf:wire(#jquery{target = "new_email_textbox", method = ["focus"]});
        false ->
            wf:wire(#jquery{target = "add_email_button", method = ["fadeIn"], args = [300]}),
            wf:wire(#jquery{target = "new_email_textbox", method = ["hide"]}),
            wf:wire(#jquery{target = "new_email_cancel", method = ["hide"]}),
            wf:wire(#jquery{target = "new_email_submit", method = ["hide"]})
    end.


% Show email adding form
show_name_edition(Flag) ->
    case Flag of
        true ->
            wf:wire(#jquery{target = "displayed_name", method = ["hide"]}),
            wf:wire(#jquery{target = "change_name_button", method = ["hide"]}),
            wf:wire(#jquery{target = "new_name_textbox", method = ["fadeIn"], args = [300]}),
            wf:wire(#jquery{target = "new_name_cancel", method = ["fadeIn"], args = [300]}),
            wf:wire(#jquery{target = "new_name_submit", method = ["fadeIn"], args = [300]}),
            wf:wire(#jquery{target = "new_name_textbox", method = ["focus"]});
        false ->
            wf:wire(#jquery{target = "displayed_name", method = ["fadeIn"], args = [300]}),
            wf:wire(#jquery{target = "change_name_button", method = ["fadeIn"], args = [300]}),
            wf:wire(#jquery{target = "new_name_textbox", method = ["hide"]}),
            wf:wire(#jquery{target = "new_name_cancel", method = ["hide"]}),
            wf:wire(#jquery{target = "new_name_submit", method = ["hide"]})
    end.


redirect_to_veilcluster() ->
    UserID = wf:user(),
%%     RedirectURL = onedata_auth:get_redirect_to_provider_url(<<"https://veilfsdev.com">>, UserID),\
    try
        RedirectURL = auth_logic:get_redirection_uri(UserID, <<"04fa9242bcdc9cb0a1bfe72d3df9054d">>),
        wf:redirect(RedirectURL)
    catch T:M ->
        ?error_stacktrace("tutut ~p:~p", [T, M])
    end.
%%     <<"veilfsdev.com/openid_login?authorization_code=", Rest/binary>> = _RedirectURL,
%%     wf:redirect(<<"https://onedata.org/auth_endpoint?authorization_code=", Rest/binary>>).


get_user_record(Key) ->
    case user_logic:get_user(Key) of
        {ok, #veil_document{record = UserRecord}} -> UserRecord;
        _ -> undefined
    end.