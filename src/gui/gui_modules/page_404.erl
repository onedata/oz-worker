%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This file contains n2o error 404 website code.
%% @end
%% ===================================================================
-module(page_404).
-author("Krzysztof Trzepla").
-compile(export_all).

% Includes
-include_lib("n2o/include/wf.hrl").
-include("registered_names.hrl").

%% Template points to the template file, which will be filled with content
main() -> #dtl{file = "bare", app = ?APP_Name, bindings = [{title, title()}, {body, body()}]}.

%% Page title
title() -> <<"Error 404">>.

%% This will be placed in the template instead of {{body}} tag
body() ->
	#panel{style = <<"position: relative;">>, body = [
		#panel{class = <<"alert alert-danger login-page">>, body = [
			#h3{body = <<"Error 404">>}
		]}
	] ++ gui_utils:logotype_footer(120)}.

event(init) -> ok.