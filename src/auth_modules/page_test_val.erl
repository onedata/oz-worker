%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2013 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains n2o website code
%% @end
%% ===================================================================

-module(page_test_val).
-compile(export_all).
-include("gui_common.hrl").
-include("logging.hrl").

%% Template points to the template file, which will be filled with content
main() -> #dtl{file = "bare", app = veil_cluster_node, bindings = [{title, title()}, {body, body()}]}.

%% Page title
title() -> <<"Val page">>.

%% This will be placed in the template instead of {{body}} tag
body() ->
    URL = auth_utils:validate_login(),
    URL.

event(init) -> ok.