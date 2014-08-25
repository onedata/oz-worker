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

-define(GR_DEPS,[sasl,lager,ssl,erlydtl,ranch,cowlib,cowboy,gproc,xmerl,ibrowse]).

-define(CREATE_DUMMY_AUTH, file:make_dir("gui_static"),file:write_file("gui_static/auth.config",<<"[].">>)).

-define(PREPARE_CERT_FILES(Config),
    begin
        GRPCADir = filename:join(?config(priv_dir,Config), "releases/data/grpca"),
        CACertsDir = filename:join(?config(priv_dir,Config), "releases/data/cacerts"),
        [{project_root, ProjectRoot}] = ets:lookup(suite_state, project_root),
        os:cmd("cp -r "++filename:join(ProjectRoot,"releases/data/grpca")++" "++GRPCADir),
        os:cmd("cp -r "++filename:join(ProjectRoot,"releases/data/cacerts")++" "++CACertsDir),
        {CACertsDir,GRPCADir}
    end).

-define(cert_paths(CACertsDir,GRPCADir),
    {ca_cert_file,filename:join(CACertsDir,"ca.crt")},
    {cert_file,filename:join(CACertsDir,"server.crt")},
    {key_file,filename:join(CACertsDir,"server.key")},
    {grpca_dir, GRPCADir},
    {rest_cert_file, filename:join(GRPCADir,"rest.pem")},
    {rest_key_file, filename:join(GRPCADir,"rest_key.pem")}).


-endif.
