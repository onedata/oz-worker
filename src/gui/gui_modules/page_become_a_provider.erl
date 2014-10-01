%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains n2o website code.
%% The page contains information how to become a provider.
%% @end
%% ===================================================================

-module(page_become_a_provider).

-include("gui/common.hrl").

% n2o API
-export([main/0, event/1]).

% URL where provider software RPM is available.
-define(DOWNLOAD_LINK, "http://packages.onedata.org/oneprovider-Linux.rpm").

% Column styles for main table
-define(FIRST_COLUMN_STYLE, <<"padding: 10px; vertical-align: top; font-size: 18px; width: 45%;">>).
-define(SECOND_COLUMN_STYLE, <<"padding: 10px; vertical-align: top; width: 24px;">>).
-define(THIRD_COLUMN_STYLE, <<"padding: 10px 10px 10px 30px; vertical-align: top; font-size: 18px;">>).
-define(LIST_ITEM_STYLE, <<"margin-bottom: 10px;">>).

%% Template points to the template file, which will be filled with content
main() ->
    #dtl{file = "bare", app = ?APP_Name, bindings = [{title, title()}, {body, body()}, {custom, <<"">>}]}.


%% Page title
title() -> <<"Become a provider">>.


%% This will be placed in the template instead of {{body}} tag
body() ->
    #panel{style = <<"position: relative;">>, body = [
        #panel{style = <<"margin-top: 5px; padding: 20px; position: relative;">>, body = [
            #h3{style = <<"padding: 10px;">>, body = <<"Become a provider">>},
            #p{style = <<"padding: 10px; font-size: 18px; width: 45%; line-height: 22.15px;">>,
                body = <<"Onedata is a global network of users sharing data spaces and providers providing shared storage space.<br />",
                "To become a provider you need to follow a few simple steps described below. To become a user go back to the main page ",
                "and log in with one of the supported authentication vendors.">>},

            #link{class = <<"btn btn-primary">>, url = <<?DOWNLOAD_LINK>>,
                style = <<"margin: 0 10px 20px; width: 300px;">>,
                body = <<"Download RPM">>},

            #h5{style = <<"padding: 0px 10px; margin-top: 30px;">>, body = <<"Installation steps">>},

            #table{style = <<"border-width: 0px; width: 100%;">>, body = [
                #tbody{body = [
                    table_row(<<"1">>, <<"Prepare a machine or cluster for Onedata software stack, having public IP address and access to your storage system.">>,
                        #list{numbered = false, body = [
                            #li{style = ?LIST_ITEM_STYLE, body = <<"The system can run on just 1 machine, although it is ",
                            "<strong>strongly recommended</strong> to use at least 4 machines for performance reasons.">>},
                            #li{style = ?LIST_ITEM_STYLE, body = <<"The software is intended for Red Hat based systems. ",
                            "It has been thoroughly tested on Scientific Linux.">>},
                            #li{style = ?LIST_ITEM_STYLE, body = <<"All hosts in the cluster must be visible to each ",
                            "other under unique, fully qualified hostnames.">>}
                        ]}),
                    table_row(<<"2">>,
                        [<<"Download and install the ">>, #link{body = <<"RPM package">>, url = <<?DOWNLOAD_LINK>>}, <<" on each node of the cluster.">>],
                        <<"After downloading the package, install it on all hosts you would like to deploy the software on.">>),
                    table_row(<<"3">>,
                        <<"Visit <strong>https://&lt;hostname&gt;:9443</strong>, where &lt;hostname&gt; is any node in the cluster">>,
                        <<"<strong>onepanel</strong> is an admin panel for the cluster. It is hosted on every",
                        " node under <strong>https://&lthostname&gt:9443</strong>. You can use any of the nodes.">>),
                    table_row(<<"4">>,
                        <<"Follow the installation instructions">>,
                        <<"When you are prompted to register as a provider, do so.">>),
                    table_row(<<"5">>,
                        <<"Congratulations, you are now a <strong>Onedata provider</strong>!">>,
                        <<"Use <strong>onepanel</strong> to grant support for spaces ",
                        "(Spaces -> Settings). You will need a token from a space owner to do so. To try it for yourself, ",
                        "log in to <strong>Onedata</strong> as a user to aquire such token.">>)
                ]}
            ]},

            #link{class = <<"btn btn-success">>, url = <<"/">>,
                style = <<"margin: 110px 10px 10px; width: 150px;">>,
                body = <<"Main page">>}
        ]},
        #panel{style = <<"float: right; width: 45%;">>, body = [
        ]},
        #panel{style = <<"clear: both;">>},
        gui_utils:cookie_policy_popup_body(<<?privacy_policy_url>>)
    ]}.


% Renders to a table row representing a single step in installation
table_row(NumberBinary, Body, AdditionalInfo) ->
    InfoCellID = <<"info", NumberBinary/binary>>,
    #tr{cells = [
        #td{style = ?FIRST_COLUMN_STYLE, body = [<<NumberBinary/binary, ". ">>, Body]},
        #td{style = ?SECOND_COLUMN_STYLE, body = #link{class = <<"glyph-link">>,
            postback = {toggle_info, InfoCellID}, body = #span{class = <<"icomoon-question">>, style = <<"font-size: 24px;">>}
        }},
        #td{style = ?THIRD_COLUMN_STYLE, body = #panel{id = InfoCellID, style = <<"display: none;">>, body = AdditionalInfo}}
    ]}.


event(init) ->
    ok;

event(terminate) ->
    ok;

event({toggle_info, InfoCellID}) ->
    case get({info, InfoCellID}) of
        true ->
            gui_jq:hide(InfoCellID),
            put({info, InfoCellID}, false);
        _ ->
            gui_jq:show(InfoCellID),
            put({info, InfoCellID}, true)
    end.


