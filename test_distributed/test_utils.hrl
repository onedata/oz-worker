%% ===================================================================
%% @author Tomasz Lichon
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Common definitions for ct tests
%% @end
%% ===================================================================
-author("Tomasz Lichon").

-include_lib("common_test/include/ct.hrl").

-ifndef(TEST_UTILS_HRL).
-define(TEST_UTILS_HRL, 1).

-define(GR_DEPS, [sasl, lager, ssl, erlydtl, ranch, cowlib, cowboy, gproc, xmerl, ibrowse]).

-define(MAKE_DIR(Root, Dir),
    begin
        lists:foldl(fun(Leaf, Path) ->
            NewPath = filename:join(Path, Leaf),
            file:make_dir(NewPath),
            NewPath
        end, Root, filename:split(Dir))
    end).

-define(CREATE_DUMMY_AUTH,
    begin
        ?MAKE_DIR(".", "releases/data/gui_static"),
        file:write_file("releases/data/gui_static/auth.config", <<"[].">>)
    end).

-define(PREPARE_CERT_FILES(Config),
    begin
        PrivDir = ?config(priv_dir, Config),
        ?MAKE_DIR(PrivDir, "releases/data"),
        GRPCADir = filename:join(PrivDir, "releases/data/grpca"),
        CACertsDir = filename:join(PrivDir, "releases/data/cacerts"),
        [{project_root, ProjectRoot}] = ets:lookup(suite_state, project_root),
        os:cmd("cp -r " ++ filename:join(ProjectRoot, "grpca") ++ " " ++ GRPCADir),
        os:cmd("cp -r " ++ filename:join(ProjectRoot, "cacerts") ++ " " ++ CACertsDir),
        {CACertsDir, GRPCADir}
    end).

-define(cert_paths(CACertsDir, GRPCADir),
    {ca_cert_file, filename:join(CACertsDir, "ca.crt")},
    {cert_file, filename:join(CACertsDir, "server.crt")},
    {key_file, filename:join(CACertsDir, "server.key")},
    {grpca_dir, GRPCADir},
    {rest_cert_file, filename:join(GRPCADir, "rest.pem")},
    {rest_key_file, filename:join(GRPCADir, "rest_key.pem")}).


-endif.
