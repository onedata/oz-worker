%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Common functions for ct tests.
%% @end
%% ===================================================================

-module(test_utils).

-include("test_utils.hrl").

%% API
-export([make_dir/2, cleanup/0]).
-export([register_provider/4]).

%% ====================================================================
%% API functions
%% ====================================================================


%% make_dir/2
%% ====================================================================
%% @doc Creates directory starting from given root.
%% @end
%% ====================================================================
-spec make_dir(Root :: string(), Dir :: string()) ->
    Path :: string().
%% ====================================================================
make_dir(Root, Dir) ->
    lists:foldl(fun(Leaf, Path) ->
        NewPath = filename:join(Path, Leaf),
        catch file:make_dir(NewPath),
        NewPath
    end, Root, filename:split(Dir)).


%% cleanup/0
%% ====================================================================
%% @doc Deletes databases and temporary files.
%% @end
%% ====================================================================
-spec cleanup() -> ok.
%% ====================================================================
cleanup() ->
    os:cmd("rm -rf " ++ ?TEMP_DIR),
    os:cmd("./delete_test_dbs.sh"),
    make_dir("/", "tmp/onedata"),
    make_dir(".", "resources"),
    file:write_file("resources/auth.config", <<"[].">>),
    ok.


%% register_provider/1
%% ====================================================================
%% @doc Registers provider in Global Registry.
%% @end
%% ====================================================================
-spec register_provider(Config :: term(), Name :: binary(), URLs :: [binary()],
    RedirectionPoint :: binary()) ->
    {ok, ProviderId :: binary(), ProviderCertPem :: binary()}.
%% ====================================================================
register_provider(Config, Name, URLs, RedirectionPoint) ->
    try
        {MegaSec, Sec, MiliSec} = erlang:now(),
        Prefix = lists:foldl(fun(Int, Acc) -> Acc ++ integer_to_list(Int) end, "provider", [MegaSec, Sec, MiliSec]),
        KeyFile = filename:join(?TEMP_DIR, Prefix ++ "_key.pem"),
        CSRFile = filename:join(?TEMP_DIR, Prefix ++ "_csr.pem"),
        CertFile = filename:join(?TEMP_DIR, Prefix ++ "_cert.pem"),
        os:cmd("openssl genrsa -out " ++ KeyFile ++ " 2048"),
        os:cmd("openssl req -new -batch -key " ++ KeyFile ++ " -out " ++ CSRFile),
        {ok, CSR} = file:read_file(CSRFile),
        [Node] = ?config(nodes, Config),
        {ok, Id, Cert} = rpc:call(Node, provider_logic, create, [Name, URLs, RedirectionPoint, CSR]),
        file:write_file(CertFile, Cert),
        {ok, Id, KeyFile, CertFile}
    catch
        _:Reason ->
            {error, Reason}
    end.
