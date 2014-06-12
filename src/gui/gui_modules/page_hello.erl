%% ===================================================================
%% @author Tomasz Lichon
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This file contains n2o hello website code.
%% @end
%% ===================================================================
-module(page_hello).
-author("Tomasz Lichon").

%% API
-compile(export_all).

% Includes
-include_lib("n2o/include/wf.hrl").
-include("registered_names.hrl").

%% Template points to the template file, which will be filled with content
main() -> #dtl{file = "bare", app = ?APP_Name, bindings = [{title, title()}, {body, body()}]}.

%% Page title
title() -> <<"Hello">>.

%% This will be placed in the template instead of {{body}} tag
body() ->
	#panel{style = <<"position: relative;">>, body = [
		#h3{body = <<"Hello">>}
	] ++ gui_utils:logotype_footer(120)}.

event(init) -> ok.

