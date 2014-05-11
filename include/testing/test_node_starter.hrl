%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2014, ACK CYFRONET AGH
%%% @doc
%%% Header file for functions used by ct test to start nodes for testing
%%% @end
%%% Created : 04. May 2014 12:15 PM
%%%-------------------------------------------------------------------
-author("Tomasz Lichon").

%% This macro adds all ebin directories needed by ct tests to code path
-define(INIT_CODE_PATH, begin
							% prepare dirs
	                        {ok, CWD} = file:get_cwd(),
	                        TestRoot = filename:join(CWD, "../.."),
	                        ProjectRoot = filename:join(TestRoot,".."),
	                        Ebin = filename:join(ProjectRoot,"ebin"),
	                        Deps = filename:join(ProjectRoot,"deps"),
	                        {ok, DepDirs} = file:list_dir(Deps),
	                        DepEbinDirs = lists:map(fun(Dir) -> filename:join([Deps,Dir,"ebin"]) end,DepDirs),

	                        % add dirs to code path
	                        code:add_path(ProjectRoot),
	                        code:add_path(TestRoot),
	                        code:add_path(Ebin),
	                        code:add_paths(DepEbinDirs)
                        end).
-define(CURRENT_HOST, begin
	                      CurrNode = atom_to_list(node()),
	                      [_, CurrHost] = string:tokens(CurrNode, "@"),
	                      list_to_atom(CurrHost)
                      end).
-define(NODE(NodeHost,NodeName), begin
	                                 list_to_atom(atom_to_list(NodeName)++"@"++atom_to_list(NodeHost))
                                 end).