%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This file contains tests for entity graph module logic.
%%% @end
%%%-------------------------------------------------------------------
-module(entity_graph_test_SUITE).
-author("Lukasz Opiola").

-include("entity_logic.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

-define(PROXY_ENDPOINT, <<"172.17.0.9:8080/api/v1">>).
-define(HANDLE_SERVICE_DATA,
    #{
        <<"name">> => <<"LifeWatch DataCite">>,
        <<"proxyEndpoint">> => ?PROXY_ENDPOINT,
        <<"serviceProperties">> => #{
            <<"type">> => <<"DOI">>,
            <<"host">> => <<"https://mds.test.datacite.org">>,
            <<"doiEndpoint">> => <<"/doi">>,
            <<"metadataEndpoint">> => <<"/metadata">>,
            <<"mediaEndpoint">> => <<"/media">>,
            <<"prefix">> => <<"10.5072">>,
            <<"username">> => <<"alice">>,
            <<"password">> => <<"*******">>,
            <<"identifierTemplate">> => <<"{{space.name}}-{{space.guid}}">>,
            <<"allowTemplateOverride">> => false
        }
    }
).
-define(HANDLE_METADATA, <<"<?xml version=\"1.0\"?>",
    "<metadata xmlns:xsi=\"http:\/\/www.w3.org\/2001\/XMLSchema-instance\" xmlns:dc=\"http:\/\/purl.org\/dc\/elements\/1.1\/\">"
    "<dc:title>Test dataset<\/dc:title>",
    "<dc:creator>John Johnson<\/dc:creator>",
    "<dc:creator>Jane Doe<\/dc:creator>",
    "<dc:subject>Test of datacite<\/dc:subject>",
    "<dc:description>Lorem ipsum lorem ipusm<\/dc:description>",
    "<dc:publisher>Onedata<\/dc:publisher>",
    "<dc:publisher>EGI<\/dc:publisher>",
    "<dc:date>2016<\/dc:date>",
    "<dc:format>application\/pdf<\/dc:format>",
    "<dc:identifier>onedata:LKJHASKFJHASLKDJHKJHuah132easd<\/dc:identifier>",
    "<dc:language>eng<\/dc:language>",
    "<dc:rights>CC-0<\/dc:rights>",
    "<\/metadata>">>).

%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([eff_users_test/1]).

all() ->
    ?ALL([
        eff_relations_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================


eff_users_test(Config) ->
    MinSupportSize = min_support_size(Config),
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    ok = oz_test_utils:set_user_oz_privileges(Config, U1, set, privileges:oz_admin()),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    ok = oz_test_utils:set_user_oz_privileges(Config, U2, set, privileges:oz_viewer()),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    ok = oz_test_utils:set_user_oz_privileges(Config, U3, set, [view_privileges]),
    {ok, U4} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U5} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"gr">>),
    {ok, G2} = oz_test_utils:create_group(Config, ?USER(U2), <<"gr">>),
    ok = oz_test_utils:set_group_oz_privileges(Config, G2, set, [
        set_privileges, view_privileges
    ]),
    {ok, G3} = oz_test_utils:create_group(Config, ?USER(U3), <<"gr">>),
    ok = oz_test_utils:set_group_oz_privileges(Config, G2, set, [remove_provider]),
    {ok, G4} = oz_test_utils:create_group(Config, ?USER(U4), <<"gr">>),
    ok = oz_test_utils:set_group_oz_privileges(Config, G2, set, privileges:oz_viewer()),
    {ok, G5} = oz_test_utils:create_group(Config, ?USER(U5), <<"gr">>),

    {ok, G2} = oz_test_utils:add_group_to_group(Config, ?ROOT, G1, G2),
    {ok, G3} = oz_test_utils:add_group_to_group(Config, ?ROOT, G1, G3),
    {ok, G4} = oz_test_utils:add_group_to_group(Config, ?ROOT, G3, G4),

    {ok, U6} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U7} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U8} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U6), <<"sp">>),
    {ok, S2} = oz_test_utils:create_space(Config, ?USER(U7), <<"sp">>),
    {ok, S3} = oz_test_utils:create_space(Config, ?USER(U8), <<"sp">>),
    oz_test_utils:ensure_eff_graph_up_to_date(Config),
    ShareData = #{
        <<"shareId">> => undefined,
        <<"name">> => <<"sh">>,
        <<"spaceId">> => undefined,
        <<"rootFileId">> => <<"rfi">>
    },
    {ok, Sh1} = oz_test_utils:create_share(Config, ?USER(U6), ShareData#{
        <<"shareId">> => <<"sh1">>, <<"spaceId">> => S1
    }),
    {ok, Sh2} = oz_test_utils:create_share(Config, ?USER(U7), ShareData#{
        <<"shareId">> => <<"sh2">>, <<"spaceId">> => S2
    }),
    {ok, Sh3} = oz_test_utils:create_share(Config, ?USER(U8), ShareData#{
        <<"shareId">> => <<"sh3">>, <<"spaceId">> => S3
    }),

    {ok, G1} = oz_test_utils:add_group_to_space(Config, ?ROOT, S1, G1),
    {ok, G3} = oz_test_utils:add_group_to_space(Config, ?ROOT, S2, G3),
    {ok, G4} = oz_test_utils:add_group_to_space(Config, ?ROOT, S3, G4),
    {ok, G5} = oz_test_utils:add_group_to_space(Config, ?ROOT, S3, G5),

    {ok, P1} = oz_test_utils:create_provider_and_certs(Config, <<"pr">>),
    {ok, P2} = oz_test_utils:create_provider_and_certs(Config, <<"pr">>),
    {ok, Token1} = oz_test_utils:space_invite_provider_token(Config, ?USER(U6), S1),
    {ok, Token2} = oz_test_utils:space_invite_provider_token(Config, ?USER(U7), S2),
    {ok, Token3} = oz_test_utils:space_invite_provider_token(Config, ?USER(U8), S3),

    {ok, S1} = oz_test_utils:support_space(Config, ?PROVIDER(P1), P1, Token1, MinSupportSize),
    {ok, S2} = oz_test_utils:support_space(Config, ?PROVIDER(P2), P2, Token2, MinSupportSize),
    {ok, S2} = oz_test_utils:support_space(Config, ?PROVIDER(P2), P2, Token3, MinSupportSize),

    {ok, HS1} = oz_test_utils:create_handle_service(
        Config, ?USER(U1), ?HANDLE_SERVICE_DATA
    ),
    {ok, HS2} = oz_test_utils:create_handle_service(
        Config, ?USER(U4), ?HANDLE_SERVICE_DATA
    ),
    {ok, HS3} = oz_test_utils:create_handle_service(
        Config, ?USER(U5), ?HANDLE_SERVICE_DATA
    ),
    {ok, G1} = oz_test_utils:add_group_to_handle_service(Config, ?USER(U1), HS1, G1),
    {ok, G3} = oz_test_utils:add_group_to_handle_service(Config, ?USER(U4), HS2, G3),
    {ok, G5} = oz_test_utils:add_group_to_handle_service(Config, ?USER(U5), HS3, G5),
    {ok, U4} = oz_test_utils:add_user_to_handle_service(Config, ?USER(U5), HS3, U4),

    oz_test_utils:ensure_eff_graph_up_to_date(Config),

    HandleData = #{
        <<"handleServiceId">> => undefined,
        <<"resourceType">> => <<"Share">>,
        <<"resourceId">> => undefined,
        <<"metadata">> => ?HANDLE_METADATA
    },
    {ok, H1} = oz_test_utils:create_handle(Config, ?USER(U2), HandleData#{
        <<"handleServiceId">> => HS1, <<"resourceId">> => Sh1
    }),
    {ok, H2} = oz_test_utils:create_handle(Config, ?USER(U2), HandleData#{
        <<"handleServiceId">> => HS2, <<"resourceId">> => Sh2
    }),
    {ok, H3} = oz_test_utils:create_handle(Config, ?USER(U2), HandleData#{
        <<"handleServiceId">> => HS3, <<"resourceId">> => Sh3
    }),

    ok.


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================


init_per_suite(Config) ->
    NewConfig = ?TEST_INIT(
        Config, ?TEST_FILE(Config, "env_desc.json"), [oz_test_utils]
    ),
    NewConfig.


end_per_suite(Config) ->
    test_node_starter:clean_environment(Config).

min_support_size(Config) ->
    {ok, MinimumSupportSize} = oz_test_utils:call_oz(
        Config, application, get_env, [oz_worker, minimum_space_support_size]
    ),
    MinimumSupportSize.