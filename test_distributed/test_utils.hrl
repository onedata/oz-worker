%% ===================================================================
%% @author Tomasz Lichon
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Common definitions for ct tests.
%% @end
%% ===================================================================
-author("Tomasz Lichon").

-include_lib("common_test/include/ct.hrl").

-ifndef(TEST_UTILS_HRL).
-define(TEST_UTILS_HRL, 1).

-define(GR_DEPS, [sasl, lager, ssl, erlydtl, ranch, cowlib, cowboy, gproc, xmerl, ibrowse, meck]).

%% temporary directory for test files
-define(TEMP_DIR, "/tmp").

%% Returns absolute path to given file using virtual CWD which equals to ct_root/common_files
-define(COMMON_FILE(File), filename:join(ets:match(suite_state, {ct_root, '$1'}) ++ ["common_files"] ++ [File])).

-define(PREPARE_CERT_FILES(Config),
    begin
        [{project_root, ProjectRoot}] = ets:lookup(suite_state, project_root),
        GRPCADir = filename:join(ProjectRoot, "grpca"),
        CertsDir = filename:join(ProjectRoot, "certs"),
        CACertsDir = filename:join(ProjectRoot, "cacerts"),
        {CertsDir, CACertsDir, GRPCADir}
    end).

-define(cert_paths(CertsDir, CACertsDir, GRPCADir),
    {gui_key_file, filename:join(CertsDir, "gui_key.pem")},
    {gui_cert_file, filename:join(CertsDir, "gui_cert.pem")},
    {gui_cacert_file, filename:join(CACertsDir, "gui_cacert.pem")},
    {grpca_dir, GRPCADir},
    {grpkey_file, filename:join(GRPCADir, "grpkey.pem")},
    {grpcert_file, filename:join(GRPCADir, "grpcert.pem")},
    {grpcacert_file, filename:join(GRPCADir, "cacert.pem")}).

-endif.
