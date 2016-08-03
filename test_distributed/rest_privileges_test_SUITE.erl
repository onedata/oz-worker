%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Integration tests of privileges REST module in onezone.
%%% @end
%%%-------------------------------------------------------------------
-module(rest_privileges_test_SUITE).
-author("Lukasz Opiola").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").


-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([oz_api_privileges_test/1]).


%%%===================================================================
%%% API functions
%%%===================================================================

all() ->
    ?ALL([
        oz_api_privileges_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================

oz_api_privileges_test(Config) ->
    put_config(Config),
    User1 = create_user(),
    Group1 = create_group_for_user(User1),
    % Unauthenticated requests should be discarded (401)
    ?assert(check_rest_call(#{
        request => #{
            method => get,
            path => [<<"/privileges/users/">>, User1]
        },
        expect => #{
            code => 401
        }
    })),
    % User without permissions cannot view the OZ API privileges (403)
    ?assert(check_rest_call(#{
        request => #{
            method => get,
            path => [<<"/privileges/users/">>, User1],
            auth => {user, User1}
        },
        expect => #{
            code => 403
        }
    })),
    % Give the user view/set privileges and check again
    set_privileges(User1, onedata_user, [view_privileges, set_privileges]),
    ?assert(check_rest_call(#{
        request => #{
            method => get,
            path => [<<"/privileges/users/">>, User1],
            auth => {user, User1}
        },
        expect => #{
            code => 200,
            body => #{<<"privileges">> => [
                <<"view_privileges">>, <<"set_privileges">>
            ]}
        }
    })),
    % Newly created users and groups must have empty OZ API privileges
    User2 = create_user(),
    Group2 = create_group_for_user(User2),
    ?assert(check_rest_call(#{
        request => #{
            method => get,
            path => [<<"/privileges/users/">>, User2],
            auth => {user, User1}
        },
        expect => #{
            code => 200,
            body => #{<<"privileges">> => []}
        }
    })),
    ?assert(check_rest_call(#{
        request => #{
            method => get,
            path => [<<"/privileges/groups/">>, Group2],
            auth => {user, User1}
        },
        expect => #{
            code => 200,
            body => #{<<"privileges">> => []}
        }
    })),
    % On behalf of User1, give perms to Group2
    % First try some wrong perms
    ?assert(check_rest_call(#{
        request => #{
            method => put,
            path => [<<"/privileges/groups/">>, Group2],
            body => #{
                <<"privileges">> => [
                    some,
                    inexistent,
                    permissions
                ]
            },
            auth => {user, User1}
        },
        expect => #{
            code => 400
        }
    })),
    % Then a correct request
    ?assert(check_rest_call(#{
        request => #{
            method => put,
            path => [<<"/privileges/groups/">>, Group2],
            body => #{<<"privileges">> => [
                <<"view_privileges">>,
                <<"set_privileges">>,
                <<"list_spaces">>,
                <<"list_providers">>,
                <<"list_providers_of_space">>,
                <<"add_member_to_space">>,
                <<"remove_member_from_space">>
            ]},
            auth => {user, User1}
        },
        expect => #{
            code => 204
        }
    })),
    % Now user 2 should be able to do some things (as he belongs to group2)
    % 1) See privileges [view_privileges]
    % Try 10 times, because user groups are resolved asynchronously and
    % it might not work momentarily.
    ?assertEqual(true, check_rest_call(#{
        request => #{
            method => get,
            path => [<<"/privileges/groups/">>, Group1],
            auth => {user, User2}
        },
        expect => #{
            code => 200,
            body => #{<<"privileges">> => []}
        }
    }), 10),
    ?assert(check_rest_call(#{
        request => #{
            method => get,
            path => [<<"/privileges/groups/">>, Group2],
            auth => {user, User2}
        },
        expect => #{
            code => 200,
            body => #{<<"privileges">> => [
                <<"view_privileges">>,
                <<"set_privileges">>,
                <<"list_spaces">>,
                <<"list_providers">>,
                <<"list_providers_of_space">>,
                <<"add_member_to_space">>,
                <<"remove_member_from_space">>
            ]}
        }
    })),
    % 2) List all the spaces in the system [list_spaces]
    User3 = create_user(),
    Space1 = create_space_for_user(User3),
    Space2 = create_space_for_user(User3),
    Space3 = create_space_for_user(User3),
    ?assert(check_rest_call(#{
        request => #{
            method => get,
            path => <<"/spaces">>,
            auth => {user, User2}
        },
        expect => #{
            code => 200,
            body => #{<<"spaces">> => [
                Space1,
                Space2,
                Space3
            ]}
        }
    })),
    % 3) List all the providers in the system [list_providers]
    Provider1 = create_provider(),
    Provider2 = create_provider(),
    Provider3 = create_provider(),
    ?assert(check_rest_call(#{
        request => #{
            method => get,
            path => <<"/providers">>,
            auth => {user, User2}
        },
        expect => #{
            code => 200,
            body => #{<<"providers">> => [
                Provider1,
                Provider2,
                Provider3
            ]}
        }
    })),
    % 4) List providers of a space [list_providers_of_space]
    support_space(Space1, User3, Provider1),
    support_space(Space1, User3, Provider3),
    ?assert(check_rest_call(#{
        request => #{
            method => get,
            path => [<<"/spaces/">>, Space1, <<"/providers">>],
            auth => {user, User2}
        },
        expect => #{
            code => 200,
            body => #{<<"providers">> => [
                Provider1,
                Provider3
            ]}
        }
    })),
    % 5) Add user to a space [add_member_to_space]
    ?assert(check_rest_call(#{
        request => #{
            method => put,
            path => [<<"/spaces/">>, Space1, <<"/users">>],
            body => #{<<"userId">> => User1},
            auth => {user, User2}
        },
        expect => #{
            code => 204
        }
    })),
    % 6) Add group to a space [add_member_to_space]
    ?assert(check_rest_call(#{
        request => #{
            method => put,
            path => [<<"/spaces/">>, Space1, <<"/groups">>],
            body => #{<<"groupId">> => Group1},
            auth => {user, User2}
        },
        expect => #{
            code => 204
        }
    })),
    % 7) Remove user from a space [remove_member_from_space]
    ?assert(check_rest_call(#{
        request => #{
            method => delete,
            path => [<<"/spaces/">>, Space1, <<"/users/">>, User1],
            auth => {user, User2}
        },
        expect => #{
            code => 204
        }
    })),
    % 8) Remove group from a space [remove_member_from_space]
    ?assert(check_rest_call(#{
        request => #{
            method => delete,
            path => [<<"/spaces/">>, Space1, <<"/groups/">>, Group1],
            auth => {user, User2}
        },
        expect => #{
            code => 204
        }
    })),

    ok.


%%%===================================================================
%%% Helper functions
%%%===================================================================

%%--------------------------------------------------------------------
%% Performs a REST call and check the output if it matches the expected.
%% Returns true when id does and mismatch details when it does not, so
%% it is strongly recommended to wrap the call to this function in an assertion.
%% Args map looks like following:
%% #{
%%    request => #{
%%      method => get, % Optional, default: get
%%      path => [<<"/parts">>, <<"/to/be">>, <<"/concatenated">>], % Mandatory
%%      headers => [{<<"key">>, <<"value">>}], % Optional, default: ct=app/json
%%      body => <<"body content">>, % Optional, default: <<"">>
%%      auth => {user, <<"uid">>} orelse none, % Optional, default: none
%%      opts => [http_client_option] % Optional, default: []
%%    },
%%    expect => #{
%%      code => 200, % Optional, by default not validated
%%      headers => [{<<"key">>, <<"value">>}], % Optional, by def. not validated
%%      body => <<"binary">> orelse #{} % Optional, by default not validated
%%      % Specifying a map here will cause validation of JSON content-wise
%%      % (if the JSON object specified by map is equal to the one in reply)
%%    }
%% }
%%--------------------------------------------------------------------
check_rest_call(ArgsMap) ->
    try
        RequestMap = maps:get(request, ArgsMap),
        ExpectMap = maps:get(expect, ArgsMap),

        ReqMethod = maps:get(method, RequestMap, get),
        ReqPath = case maps:get(path, RequestMap) of
            Bin when is_binary(Bin) ->
                [Bin];
            List ->
                List
        end,
        ReqHeaders = maps:get(headers, RequestMap, [
            {<<"content-type">>, <<"application/json">>}
        ]),
        ReqBody = case maps:get(body, RequestMap, <<"">>) of
            Bin2 when is_binary(Bin2) ->
                Bin2;
            Map2 when is_map(Map2) ->
                json_utils:encode_map(Map2)
        end,
        ReqAuth = maps:get(auth, RequestMap, none),
        ReqOpts = maps:get(opts, RequestMap, []),

        ExpCode = maps:get(code, ExpectMap, undefined),
        ExpHeaders = maps:get(headers, ExpectMap, undefined),
        ExpBody = maps:get(body, ExpectMap, undefined),

        URL = str_utils:join_binary([get_oz_url() | ReqPath], <<"">>),
        HeadersPlusAuth = case ReqAuth of
            none ->
                ReqHeaders;
            {user, UserId} ->
                [{<<"macaroon">>, get_user_auth(UserId)} | ReqHeaders]
        end,
        ct:print("ReqURL: ~s", [URL]),
        ct:print("ReqHeaders: ~p", [HeadersPlusAuth]),
        % Add insecure option - we do not want the GR server cert to be checked.
        {ok, RespCode, RespHeaders, RespBody} = http_client:request(
            ReqMethod, URL, HeadersPlusAuth, ReqBody, [insecure | ReqOpts]
        ),

        ct:print("RespCode: ~B", [RespCode]),
        ct:print("RespHeaders: ~p", [RespHeaders]),
        ct:print("RespBody: ~s", [RespBody]),

        ct:print("---------------------------------~n~n"),

        % Check response code if specified
        case ExpCode of
            undefined ->
                ok;
            _ ->
                case RespCode of
                    ExpCode ->
                        ok;
                    _ ->
                        throw({code, RespCode, ExpCode})
                end
        end,

        % Check response headers if specified
        case ExpHeaders of
            undefined ->
                ok;
            _ ->
                NormExpHeaders = normalize_headers(ExpHeaders),
                NormRespHeaders = normalize_headers(RespHeaders),
                case NormRespHeaders of
                    NormExpHeaders ->
                        ok;
                    _ ->
                        throw({headers, NormRespHeaders, NormExpHeaders})
                end
        end,

        % Check response body if specified
        case ExpBody of
            undefined ->
                ok;
            Bin3 when is_binary(Bin3) ->
                case RespBody of
                    ExpBody ->
                        ok;
                    _ ->
                        throw({body, RespBody, ExpBody})
                end;
            Map3 when is_map(Map3) ->
                RespBodyMap = json_utils:decode_map(RespBody),
                case compare_maps(RespBodyMap, ExpBody) of
                    true ->
                        ok;
                    false ->
                        throw({body, RespBodyMap, ExpBody})
                end
        end,

        % Everything OK, return true
        true
    catch
        % Something wrong, return details. If assert is used, the test will fail
        % and properly display the point of failure.
        {Type, Actual, Expected} ->
            {
                Type,
                {got, Actual},
                {expected, Expected}
            }
    end.


create_user() ->
    Config = get_config(),
    [Node | _] = ?config(oz_worker_nodes, Config),
    {ok, UserId} = rpc:call(
        Node, user_logic, create, [#onedata_user{name = <<"whatever">>}]
    ),
    UserId.


create_group_for_user(UserId) ->
    Config = get_config(),
    [Node | _] = ?config(oz_worker_nodes, Config),
    {ok, GroupId} = rpc:call(
        Node, group_logic, create, [UserId, <<"whatever">>, role]
    ),
    GroupId.


create_space_for_user(UserId) ->
    Config = get_config(),
    [Node | _] = ?config(oz_worker_nodes, Config),
    {ok, SpaceId} = rpc:call(
        Node, space_logic, create, [{user, UserId}, <<"whatever">>]
    ),
    SpaceId.


create_provider() ->
    [Node | _] = ?config(oz_worker_nodes, get_config()),
    % Generate CSR file
    Prefix = "provider" ++ integer_to_list(random:uniform(123345123)),
    KeyFile = filename:join(?TEMP_DIR, Prefix ++ "_key.pem"),
    CSRFile = filename:join(?TEMP_DIR, Prefix ++ "_csr.pem"),
    os:cmd("openssl genrsa -out " ++ KeyFile ++ " 2048"),
    os:cmd("openssl req -new -batch -key " ++ KeyFile ++ " -out " ++ CSRFile),
    {ok, CSR} = file:read_file(CSRFile),
    {ok, ProviderId, _} = rpc:call(
        Node, provider_logic, create, [
            <<"whatever">>,
            [<<"127.0.0.1">>],
            <<"127.0.0.1">>,
            CSR
        ]
    ),
    ProviderId.


support_space(SpaceId, UserId, ProviderId) ->
    [Node | _] = ?config(oz_worker_nodes, get_config()),
    Client = #client{type = user, id = UserId},
    {ok, MacaroonBin} = rpc:call(
        Node, token_logic, create, [
            Client, space_support_token, {space, SpaceId}
        ]
    ),
    {ok, Macaroon} = rpc:call(
        Node, token_utils, deserialize, [MacaroonBin]
    ),
    {ok, SpaceId} = rpc:call(
        Node, space_logic, support, [ProviderId, Macaroon, 10000000]
    ),
    SpaceId.


set_privileges(EntityId, EntityType, Privs) ->
    [Node | _] = ?config(oz_worker_nodes, get_config()),
    rpc:call(
        Node, oz_api_privileges_logic, modify, [EntityId, EntityType, Privs]
    ).


get_user_auth(UserId) ->
    % Cache user auth tokens, if none in cache create a new one.
    case get({macaroon, UserId}) of
        undefined ->
            [Node | _] = ?config(oz_worker_nodes, get_config()),
            Macaroon = rpc:call(
                Node, auth_logic, gen_token, [UserId]
            ),
            put({macaroon, UserId}, Macaroon),
            Macaroon;
        Macaroon ->
            Macaroon
    end.


get_oz_url() ->
    Config = get_config(),
    RestURLs = ?config(restURLs, Config),
    random:seed(erlang:timestamp()),
    lists:nth(random:uniform(length(RestURLs)), RestURLs).


get_node_ip(Node) ->
    CMD = string:join([
        "docker inspect",
        "--format '{{ .NetworkSettings.IPAddress }}'",
        utils:get_host(Node)
    ], " "),
    re:replace(os:cmd(CMD), "\\s+", "", [global, {return, binary}]).


% Convert all header keys to lowercase so comparing is easier
normalize_headers(Headers) ->
    lists:sort(
        lists:map(fun({Key, Value}) ->
            KeyLower = list_to_binary(string:to_lower(binary_to_list(Key))),
            {KeyLower, Value}
        end, Headers)
    ).


% Returns true if two maps have the same contents
compare_maps(Map1, Map2) ->
    sort_map(Map1) =:= sort_map(Map2).


% Sorts all nested lists in a map and returns the result map
sort_map(OriginalMap) ->
    lists:foldl(
        fun(Key, MapAcc) ->
            case maps:get(Key, MapAcc) of
                List when is_list(List) ->
                    maps:put(Key, lists:sort(List), MapAcc);
                Map when is_map(Map) ->
                    maps:put(Key, sort_map(Map), MapAcc);
                _ ->
                    MapAcc
            end
        end, OriginalMap, maps:keys(OriginalMap)).



put_config(Config) ->
    put(config, Config).


get_config() ->
    get(config).

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    application:start(etls),
    hackney:start(),
    NewConfig = ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")),
    Nodes = ?config(oz_worker_nodes, NewConfig),
    RestURLs = lists:map(fun(Node) ->
        NodeIP = get_node_ip(Node),
        {ok, RestPort} = rpc:call(
            Node, application, get_env, [?APP_Name, rest_port]
        ),
        {ok, RestAPIPrefix} = rpc:call(
            Node, application, get_env, [?APP_Name, rest_api_prefix]
        ),
        str_utils:format_bin(
            "https://~s:~B~s", [NodeIP, RestPort, RestAPIPrefix]
        )
    end, Nodes),
    [{restURLs, RestURLs} | NewConfig].

end_per_suite(Config) ->
    hackney:stop(),
    application:stop(etls),
    ok.
%%    test_node_starter:clean_environment(Config).

