%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains n2o website code
%% @end
%% ===================================================================

-module(page_manage_account).
-compile(export_all).

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
            #panel{id = <<"main_table">>, body = main_table()}
        ]}
    ] ++ gui_utils:logotype_footer(20)}.


main_table() ->
    #table{style = <<"border-width: 0px; width: auto;">>, body = [
        #tbody{body = [
            #tr{cells = [
                #td{style = <<"border-width: 0px; padding: 10px 10px">>, body =
                #label{class = <<"label label-large label-inverse">>, style = <<"cursor: auto;">>, body = <<"Name">>}},
                #td{style = <<"border-width: 0px; padding: 10px 10px">>, body = #p{body = get_user_name()}}
            ]},

            #tr{cells = [
                #td{style = <<"border-width: 0px; padding: 10px 10px; vertical-align: top;">>, body =
                #label{class = <<"label label-large label-inverse">>, style = <<"cursor: auto;">>, body = <<"E-mails">>}},
                #td{style = <<"border-width: 0px; padding: 10px 10px">>, body = get_user_emails()}
            ]},

            #tr{cells = [
                #td{style = <<"border-width: 0px; padding: 10px 10px;  vertical-align: top;">>, body =
                #label{class = <<"label label-large label-inverse">>, style = <<"cursor: auto;">>, body = <<"Connected<br />accounts">>}},
                #td{style = <<"border-width: 0px; padding: 10px 10px">>, body = get_connected_accounts_table()}
            ]}
        ]}
    ]}.


get_user_name() ->
    #user_info{preferred_name = Name} = temp_user_logic:get_user({global, wf:user()}),
    Name.


% HTML list with emails printed
get_user_emails() ->
    #user_info{emails = Emails} = temp_user_logic:get_user({global, wf:user()}),
    #list{numbered = true, body =
    lists:map(
        fun(Email) ->
            #li{style = <<"font-size: 18px; padding: 0 0 10px;">>, body = #span{body =
            [
                Email,
                #link{id = <<"remove_email_button">>, class = <<"glyph-link">>, style = <<"margin-left: 10px;">>,
                    postback = a, body =
                    #span{class = <<"fui-cross">>, style = <<"font-size: 16px;">>}}
            ]}}
        end, Emails)
    ++ [
        #li{style = <<"font-size: 18px; padding: 5px 0;">>, body = [
            #link{id = <<"add_email_button">>, class = <<"glyph-link">>, style = <<"margin-left: 10px;">>,
                postback = {action, show_email_adding, [true]}, body =
                #span{class = <<"fui-plus">>, style = <<"font-size: 16px; position: relative;">>}},
            #textbox{id = <<"new_email_textbox">>, class = <<"flat">>, body = <<"">>, style = <<"display: none;">>,
                placeholder = <<"New email address">>, postback = a,
                source = ["new_email_textbox"]},
            #link{id = <<"new_email_submit">>, class = <<"glyph-link">>, style = <<"display: none; margin-left: 10px;">>,
                postback = a, source = ["new_email_textbox"], body =
                #span{class = <<"fui-check-inverted">>, style = <<"font-size: 20px;">>}},
            #link{id = <<"new_email_cancel">>, class = <<"glyph-link">>, style = <<"display: none; margin-left: 10px;">>,
                postback = {action, show_email_adding, [false]}, body =
                #span{class = <<"fui-cross-inverted">>, style = <<"font-size: 20px;">>}}
        ]}
    ]}.


get_connected_accounts_table() ->
    #user_info{provider_infos = ProviderInfos} = temp_user_logic:get_user({global, wf:user()}),
    TableHead = #tr{cells = [
        #th{body = <<"">>},
        #th{body = <<"provider">>},
        #th{body = <<"e-mails">>},
        #th{body = <<"login">>},
        #th{body = <<"name">>}
    ]},
    TableBody = lists:map(
        fun(Provider) ->
            ProviderInfo = find_provider_info(Provider, ProviderInfos),
            ProviderName = auth_utils:get_provider_name(Provider),

            % Checkbox
            {CheckboxIcon, CheckboxTitle, CheckboxStyle} =
                case ProviderInfo of
                    undefined ->
                        {<<"fui-checkbox-unchecked">>,
                            <<"Connect ", ProviderName/binary, " account">>,
                            <<"line-height: 32px; font-size: 25px; color: rgb(200,200,200);">>};
                    _ ->
                        {<<"fui-checkbox-checked">>,
                            <<"Disonnect ", ProviderName/binary, " account">>,
                            <<"line-height: 32px; font-size: 25px; color: #1ABC9C;">>}
                end,

            Checkbox = #link{title = CheckboxTitle, style = CheckboxStyle,
                postback = {toggle_account, Provider}, body = #span{class = CheckboxIcon}},

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
                         #provider_user_info{emails = List} ->
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
                        #provider_user_info{login = SomeLogin} ->
                            case SomeLogin of
                                <<"">> -> <<"-">>;
                                _ -> SomeLogin
                            end
                    end,

            % Login
            Name = case ProviderInfo of
                       undefined ->
                           <<"">>;
                       #provider_user_info{name = SomeName} ->
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


find_provider_info(Provider, ProviderInfos) ->
    lists:foldl(fun(ProvUserInfo = #provider_user_info{provider_id = ProviderID}, Acc) ->
        case Provider of
            ProviderID -> ProvUserInfo;
            _ -> Acc
        end
    end, undefined, ProviderInfos).


% Postback event handling
event(init) -> ok;

event({toggle_account, Provider}) ->
    % Get user info doc
    #user_info{provider_infos = ProviderInfos} = temp_user_logic:get_user({global, wf:user()}),
    % Get provider name
    ProviderName = auth_utils:get_provider_name(Provider),
    % Get provider info from user info doc
    ProviderInfo = find_provider_info(Provider, ProviderInfos),
    case ProviderInfo of
        undefined ->
            % The user hasn't yet connected his account from this provider, do it
            HandlerModule = auth_utils:get_provider_module(Provider),
            {ok, URL} = HandlerModule:get_redirect_url(true),
            wf:redirect(URL);
        _ ->
            % The user has already connected his account from this provider
            case length(ProviderInfos) of
                1 ->
                    % Prevent from disconnecting last account
                    wf:wire(#alert{text = <<"You cannot disconnect your last account.">>});
                _ ->
                    % Prompt for confirmation to delete
                    wf:wire(#confirm{text = <<"Are you sure you want to disconnect your ", ProviderName/binary, " account?">>,
                        postback = {disconnect, Provider}})
            end
    end;

event({disconnect, Provider}) ->
    GlobalID = wf:user(),
    % Find the user, remove provider info from his user info doc and reload the page
    UserInfo = #user_info{provider_infos = ProviderInfos} = temp_user_logic:get_user({global, GlobalID}),
    ProviderInfo = find_provider_info(Provider, ProviderInfos),
    temp_user_logic:update_user({global, GlobalID},
        UserInfo#user_info{provider_infos = ProviderInfos -- [ProviderInfo]}),
    wf:redirect(<<"/manage_account">>).



