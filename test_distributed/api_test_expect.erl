%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module gathers all definitions of expected responses returned by GET
%%% API operations for different entities, scopes and interfaces.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(api_test_expect).
-author("Lukasz Opiola").

-include("api_test_utils.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/graph_sync/gri.hrl").
-include_lib("ctool/include/test/assertions.hrl").

-type interface() :: logic | rest | gs.
-type expectation() :: logic_expectation() | rest_expectation() | gs_expectation().


%% API
-export([protected_user/3, shared_user/3]).
-export([protected_group/4, shared_group/3]).
-export([protected_space/4]).
-export([private_share/4, public_share/3]).
-export([protected_provider/3, shared_provider/3]).
-export([protected_cluster/4, public_cluster/3]).
-export([protected_hservice/4]).
-export([protected_handle/4, public_handle/3]).
-export([protected_harvester/4, shared_or_public_harvester/3]).
-export([protected_atm_inventory/4]).
-export([private_atm_lambda/4]).

%%%===================================================================
%%% API
%%%===================================================================

-spec protected_user(interface(), od_user:id(), map()) -> expectation().
protected_user(logic, _Id, UserData) ->
    ?OK_MAP(#{
        <<"basicAuthEnabled">> => maps:get(<<"basicAuthEnabled">>, UserData, false),
        <<"fullName">> => maps:get(<<"fullName">>, UserData),
        <<"username">> => maps:get(<<"username">>, UserData),
        <<"emails">> => maps:get(<<"emails">>, UserData, []),
        <<"linkedAccounts">> => [],
        <<"blocked">> => maps:get(<<"blocked">>, UserData, false),
        <<"creationTime">> => ozt_mocks:get_frozen_time_seconds()
    });
protected_user(rest, Id, UserData) ->
    #{
        <<"userId">> => Id,
        <<"basicAuthEnabled">> => maps:get(<<"basicAuthEnabled">>, UserData, false),
        <<"fullName">> => maps:get(<<"fullName">>, UserData),
        <<"username">> => maps:get(<<"username">>, UserData),
        <<"emails">> => maps:get(<<"emails">>, UserData, []),
        <<"linkedAccounts">> => [],
        <<"blocked">> => maps:get(<<"blocked">>, UserData, false),
        <<"creationTime">> => ozt_mocks:get_frozen_time_seconds(),
        % TODO VFS-4506 deprecated fields, included for backward compatibility
        <<"name">> => maps:get(<<"fullName">>, UserData),
        <<"login">> => maps:get(<<"username">>, UserData),
        <<"alias">> => maps:get(<<"username">>, UserData),
        <<"emailList">> => maps:get(<<"emails">>, UserData, [])
    };
protected_user(gs, Id, UserData) ->
    ?OK_MAP(#{
        <<"gri">> => gri:serialize(?GRI(od_user, Id, instance, protected)),
        <<"fullName">> => maps:get(<<"fullName">>, UserData),
        <<"username">> => maps:get(<<"username">>, UserData),
        <<"emails">> => maps:get(<<"emails">>, UserData, []),
        <<"linkedAccounts">> => [],
        <<"blocked">> => maps:get(<<"blocked">>, UserData, false),
        % TODO VFS-4506 deprecated fields, included for backward compatibility
        <<"name">> => maps:get(<<"fullName">>, UserData),
        <<"login">> => maps:get(<<"username">>, UserData),
        <<"alias">> => maps:get(<<"username">>, UserData),
        <<"emailList">> => maps:get(<<"emails">>, UserData, [])
    }).


-spec shared_user(interface(), od_user:id(), map()) -> expectation().
shared_user(logic, _Id, UserData) ->
    ?OK_MAP(#{
        <<"fullName">> => maps:get(<<"fullName">>, UserData),
        <<"username">> => maps:get(<<"username">>, UserData),
        <<"creationTime">> => ozt_mocks:get_frozen_time_seconds()
    });
shared_user(rest, Id, UserData) ->
    #{
        <<"userId">> => Id,
        <<"fullName">> => maps:get(<<"fullName">>, UserData),
        <<"username">> => maps:get(<<"username">>, UserData),
        <<"creationTime">> => ozt_mocks:get_frozen_time_seconds(),
        % TODO VFS-4506 deprecated fields, included for backward compatibility
        <<"name">> => maps:get(<<"fullName">>, UserData),
        <<"login">> => maps:get(<<"username">>, UserData),
        <<"alias">> => maps:get(<<"username">>, UserData)
    };
shared_user(gs, Id, UserData) ->
    ?OK_MAP(#{
        <<"gri">> => gri:serialize(?GRI(od_user, Id, instance, shared)),
        <<"fullName">> => maps:get(<<"fullName">>, UserData),
        <<"username">> => maps:get(<<"username">>, UserData),
        % TODO VFS-4506 deprecated fields, included for backward compatibility
        <<"name">> => maps:get(<<"fullName">>, UserData),
        <<"login">> => maps:get(<<"username">>, UserData),
        <<"alias">> => maps:get(<<"username">>, UserData)
    }).


-spec protected_group(interface(), od_group:id(), map(), aai:subject()) -> expectation().
protected_group(logic, _Id, GroupData, Creator) ->
    ?OK_MAP(#{
        <<"name">> => maps:get(<<"name">>, GroupData),
        <<"type">> => maps:get(<<"type">>, GroupData),
        <<"creationTime">> => ozt_mocks:get_frozen_time_seconds(),
        <<"creator">> => Creator
    });
protected_group(rest, Id, GroupData, Creator) ->
    #{
        <<"groupId">> => Id,
        <<"name">> => maps:get(<<"name">>, GroupData),
        <<"type">> => atom_to_binary(maps:get(<<"type">>, GroupData), utf8),
        <<"creationTime">> => ozt_mocks:get_frozen_time_seconds(),
        <<"creator">> => aai:subject_to_json(Creator)
    };
protected_group(gs, Id, GroupData, _Creator) ->
    ?OK_MAP(#{
        <<"gri">> => gri:serialize(?GRI(od_group, Id, instance, protected)),
        <<"name">> => maps:get(<<"name">>, GroupData),
        <<"type">> => atom_to_binary(maps:get(<<"type">>, GroupData), utf8)
    }).


-spec shared_group(interface(), od_group:id(), map()) -> expectation().
shared_group(logic, _Id, GroupData) ->
    ?OK_MAP(#{
        <<"name">> => maps:get(<<"name">>, GroupData),
        <<"type">> => maps:get(<<"type">>, GroupData),
        <<"creationTime">> => ozt_mocks:get_frozen_time_seconds()
    });
shared_group(rest, Id, GroupData) ->
    #{
        <<"groupId">> => Id,
        <<"name">> => maps:get(<<"name">>, GroupData),
        <<"type">> => atom_to_binary(maps:get(<<"type">>, GroupData), utf8),
        <<"creationTime">> => ozt_mocks:get_frozen_time_seconds()
    };
shared_group(gs, Id, GroupData) ->
    ?OK_MAP(#{
        <<"gri">> => gri:serialize(?GRI(od_group, Id, instance, shared)),
        <<"name">> => maps:get(<<"name">>, GroupData),
        <<"type">> => atom_to_binary(maps:get(<<"type">>, GroupData), utf8)
    }).


-spec protected_space(interface(), od_space:id(), map(), aai:subject()) -> expectation().
protected_space(logic, _Id, SpaceData, Creator) ->
    ?OK_MAP(#{
        <<"name">> => maps:get(<<"name">>, SpaceData),
        <<"providers">> => maps:get(<<"providers">>, SpaceData, #{}),
        <<"creationTime">> => ozt_mocks:get_frozen_time_seconds(),
        <<"creator">> => Creator,
        <<"sharesCount">> => 0
    });
protected_space(rest, Id, SpaceData, Creator) ->
    #{
        <<"spaceId">> => Id,
        <<"name">> => maps:get(<<"name">>, SpaceData),
        <<"providers">> => maps:get(<<"providers">>, SpaceData, #{}),
        <<"creationTime">> => ozt_mocks:get_frozen_time_seconds(),
        <<"creator">> => aai:subject_to_json(Creator)
    };
protected_space(gs, Id, SpaceData, _Creator) ->
    ?OK_MAP(#{
        <<"gri">> => gri:serialize(?GRI(od_space, Id, instance, protected)),
        <<"name">> => maps:get(<<"name">>, SpaceData),
        <<"providers">> => maps:get(<<"providers">>, SpaceData, #{})
    }).


-spec private_share(interface(), od_share:id(), map(), aai:subject()) -> expectation().
private_share(logic, _Id, ShareData, Creator) ->
    ?OK_TERM(fun(ShareRecord) ->
        ?assertEqual(ShareRecord, #od_share{
            name = maps:get(<<"name">>, ShareData),
            description = maps:get(<<"description">>, ShareData),

            space = maps:get(<<"spaceId">>, ShareData),
            handle = maps:get(<<"handleId">>, ShareData, undefined),

            root_file = maps:get(<<"rootFileId">>, ShareData),
            file_type = maps:get(<<"fileType">>, ShareData),

            creation_time = ozt_mocks:get_frozen_time_seconds(),
            creator = Creator
        })
    end);
private_share(rest, Id, ShareData, Creator) ->
    #{
        <<"shareId">> => Id,
        <<"name">> => maps:get(<<"name">>, ShareData),
        <<"description">> => maps:get(<<"description">>, ShareData),
        <<"spaceId">> => maps:get(<<"spaceId">>, ShareData),
        <<"rootFileId">> => element(2, {ok, _} = file_id:guid_to_objectid(maps:get(<<"rootFileId">>, ShareData))),
        <<"fileType">> => atom_to_binary(maps:get(<<"fileType">>, ShareData), utf8),
        <<"handleId">> =>  utils:undefined_to_null(maps:get(<<"handleId">>, ShareData, undefined)),
        <<"publicUrl">> => expected_public_share_url(Id),
        <<"publicRestUrl">> => expected_public_share_rest_url(Id),
        <<"creationTime">> => ozt_mocks:get_frozen_time_seconds(),
        <<"creator">> => aai:subject_to_json(Creator)
    };
private_share(gs, Id, ShareData, _Creator) ->
    ?OK_MAP(#{
        <<"gri">> => gri:serialize(?GRI(od_share, Id, instance, private)),
        <<"name">> => maps:get(<<"name">>, ShareData),
        <<"description">> => maps:get(<<"description">>, ShareData),
        <<"spaceId">> => maps:get(<<"spaceId">>, ShareData),
        <<"rootFileId">> => maps:get(<<"rootFileId">>, ShareData),
        <<"fileType">> => atom_to_binary(maps:get(<<"fileType">>, ShareData), utf8),
        <<"handleId">> =>  utils:undefined_to_null(maps:get(<<"handleId">>, ShareData, undefined)),
        <<"publicUrl">> => expected_public_share_url(Id),
        <<"publicRestUrl">> => expected_public_share_rest_url(Id)
    }).


-spec public_share(interface(), od_share:id(), map()) -> expectation().
public_share(logic, _Id, ShareData) ->
    ?OK_MAP(#{
        <<"spaceId">> => maps:get(<<"spaceId">>, ShareData),
        <<"name">> => maps:get(<<"name">>, ShareData),
        <<"description">> => maps:get(<<"description">>, ShareData),
        <<"rootFileId">> => maps:get(<<"rootFileId">>, ShareData),
        <<"fileType">> => maps:get(<<"fileType">>, ShareData),
        <<"handleId">> => maps:get(<<"handleId">>, ShareData, undefined),
        <<"creationTime">> => ozt_mocks:get_frozen_time_seconds()
    });
public_share(rest, Id, ShareData) ->
    #{
        <<"shareId">> => Id,
        <<"name">> => maps:get(<<"name">>, ShareData),
        <<"description">> => maps:get(<<"description">>, ShareData),
        <<"rootFileId">> => element(2, {ok, _} = file_id:guid_to_objectid(maps:get(<<"rootFileId">>, ShareData))),
        <<"fileType">> => atom_to_binary(maps:get(<<"fileType">>, ShareData), utf8),
        <<"handleId">> =>  utils:undefined_to_null(maps:get(<<"handleId">>, ShareData, undefined)),
        <<"publicUrl">> => expected_public_share_url(Id),
        <<"publicRestUrl">> => expected_public_share_rest_url(Id),
        <<"creationTime">> => ozt_mocks:get_frozen_time_seconds()
    };
public_share(gs, Id, ShareData) ->
    ?OK_MAP(#{
        <<"gri">> => gri:serialize(?GRI(od_share, Id, instance, public)),
        <<"spaceId">> => maps:get(<<"spaceId">>, ShareData),
        <<"name">> => maps:get(<<"name">>, ShareData),
        <<"description">> => maps:get(<<"description">>, ShareData),
        <<"rootFileId">> => maps:get(<<"rootFileId">>, ShareData),
        <<"fileType">> => atom_to_binary(maps:get(<<"fileType">>, ShareData), utf8),
        <<"handleId">> =>  utils:undefined_to_null(maps:get(<<"handleId">>, ShareData, undefined)),
        <<"publicUrl">> => expected_public_share_url(Id),
        <<"publicRestUrl">> => expected_public_share_rest_url(Id)
    }).


-spec protected_provider(interface(), od_provider:id(), map()) -> expectation().
protected_provider(logic, _Id, ProviderData) ->
    ?OK_MAP(#{
        <<"name">> => maps:get(<<"name">>, ProviderData),
        <<"domain">> => maps:get(<<"domain">>, ProviderData),
        <<"latitude">> => maps:get(<<"latitude">>, ProviderData, 0.0),
        <<"longitude">> => maps:get(<<"longitude">>, ProviderData, 0.0),
        <<"online">> => maps:get(<<"online">>, ProviderData, false),
        <<"creationTime">> => ozt_mocks:get_frozen_time_seconds()
    });
protected_provider(rest, Id, ProviderData) ->
    #{
        <<"providerId">> => Id,
        <<"clusterId">> => Id,
        <<"name">> => maps:get(<<"name">>, ProviderData),
        <<"domain">> => maps:get(<<"domain">>, ProviderData),
        <<"latitude">> => maps:get(<<"latitude">>, ProviderData),
        <<"longitude">> => maps:get(<<"longitude">>, ProviderData),
        <<"online">> => maps:get(<<"online">>, ProviderData, false),
        <<"creationTime">> => ozt_mocks:get_frozen_time_seconds()
    };
protected_provider(gs, Id, ProviderData) ->
    ?OK_MAP(#{
        <<"gri">> => gri:serialize(?GRI(od_provider, Id, instance, protected)),
        <<"name">> => maps:get(<<"name">>, ProviderData),
        <<"domain">> => maps:get(<<"domain">>, ProviderData),
        <<"latitude">> => maps:get(<<"latitude">>, ProviderData),
        <<"longitude">> => maps:get(<<"longitude">>, ProviderData),
        <<"online">> => maps:get(<<"online">>, ProviderData, false)
    }).


-spec shared_provider(interface(), od_provider:id(), map()) -> expectation().
shared_provider(logic, _Id, ProviderData) ->
    ?OK_MAP(#{
        <<"name">> => maps:get(<<"name">>, ProviderData)
    }).


-spec protected_cluster(interface(), od_cluster:id(), od_cluster:type(), aai:subject()) -> expectation().
protected_cluster(logic, _Id, Type, Creator) ->
    ?OK_MAP(#{
        <<"type">> => Type,
        <<"workerVersion">> => expected_cluster_worker_version_info(Type),
        <<"onepanelVersion">> => expected_cluster_panel_version_info(),
        <<"onepanelProxy">> => true,
        <<"creationTime">> => expected_cluster_creation_time(Type),
        <<"creator">> => Creator
    });
protected_cluster(rest, Id, Type, Creator) ->
    #{
        <<"clusterId">> => Id,
        <<"type">> => atom_to_binary(Type, utf8),
        <<"workerVersion">> => expected_cluster_worker_version_info(Type),
        <<"onepanelVersion">> => expected_cluster_panel_version_info(),
        <<"onepanelProxy">> => true,
        <<"creationTime">> => expected_cluster_creation_time(Type),
        <<"creator">> => aai:subject_to_json(Creator)
    }.


-spec public_cluster(interface(), od_cluster:id(), od_cluster:type()) -> expectation().
public_cluster(logic, _Id, Type) ->
    ?OK_MAP(#{
        <<"type">> => Type,
        <<"workerVersion">> => expected_cluster_worker_version_info(Type),
        <<"onepanelVersion">> => expected_cluster_panel_version_info(),
        <<"creationTime">> => expected_cluster_creation_time(Type)
    });
public_cluster(rest, Id, Type) ->
    #{
        <<"clusterId">> => Id,
        <<"type">> => atom_to_binary(Type, utf8),
        <<"workerVersion">> => expected_cluster_worker_version_info(Type),
        <<"onepanelVersion">> => expected_cluster_panel_version_info(),
        <<"creationTime">> => expected_cluster_creation_time(Type)
    }.


-spec protected_hservice(interface(), od_handle_service:id(), map(), aai:subject()) -> expectation().
protected_hservice(logic, _Id, HServiceData, Creator) ->
    ?OK_MAP(#{
        <<"name">> => maps:get(<<"name">>, HServiceData),
        <<"proxyEndpoint">> => maps:get(<<"proxyEndpoint">>, HServiceData),
        <<"serviceProperties">> => maps:get(<<"serviceProperties">>, HServiceData),
        <<"creationTime">> => ozt_mocks:get_frozen_time_seconds(),
        <<"creator">> => Creator
    });
protected_hservice(rest, Id, HServiceData, Creator) ->
    #{
        <<"handleServiceId">> => Id,
        <<"name">> => maps:get(<<"name">>, HServiceData),
        <<"proxyEndpoint">> => maps:get(<<"proxyEndpoint">>, HServiceData),
        <<"serviceProperties">> => maps:get(<<"serviceProperties">>, HServiceData),
        <<"creationTime">> => ozt_mocks:get_frozen_time_seconds(),
        <<"creator">> => aai:subject_to_json(Creator)
    }.


-spec protected_handle(interface(), od_handle:id(), map(), aai:subject()) -> expectation().
protected_handle(logic, Id, HandleData, Creator) ->
    ?OK_MAP(#{
        <<"handleServiceId">> => maps:get(<<"handleServiceId">>, HandleData),
        <<"publicHandle">> => expected_public_handle(Id),
        <<"resourceType">> => maps:get(<<"resourceType">>, HandleData, <<"Share">>),
        <<"resourceId">> => maps:get(<<"resourceId">>, HandleData),
        <<"metadata">> => maps:get(<<"metadata">>, HandleData),
        <<"timestamp">> => ozt_mocks:get_frozen_time_seconds(),
        <<"creationTime">> => ozt_mocks:get_frozen_time_seconds(),
        <<"creator">> => Creator
    });
protected_handle(rest, Id, HandleData, Creator) ->
    #{
        <<"handleId">> => Id,
        <<"handleServiceId">> => maps:get(<<"handleServiceId">>, HandleData),
        <<"publicHandle">> => expected_public_handle(Id),
        <<"resourceType">> => maps:get(<<"resourceType">>, HandleData, <<"Share">>),
        <<"resourceId">> => maps:get(<<"resourceId">>, HandleData),
        <<"metadata">> => maps:get(<<"metadata">>, HandleData),
        <<"timestamp">> => time:seconds_to_iso8601(ozt_mocks:get_frozen_time_seconds()),
        <<"creationTime">> => ozt_mocks:get_frozen_time_seconds(),
        <<"creator">> => aai:subject_to_json(Creator)
    }.


-spec public_handle(interface(), od_handle:id(), map()) -> expectation().
public_handle(logic, Id, HandleData) ->
    ?OK_MAP(#{
        <<"publicHandle">> => expected_public_handle(Id),
        <<"resourceType">> => maps:get(<<"resourceType">>, HandleData, <<"Share">>),
        <<"resourceId">> => maps:get(<<"resourceId">>, HandleData),
        <<"metadata">> => maps:get(<<"metadata">>, HandleData),
        <<"timestamp">> => ozt_mocks:get_frozen_time_seconds(),
        <<"creationTime">> => ozt_mocks:get_frozen_time_seconds()
    });
public_handle(rest, Id, HandleData) ->
    #{
        <<"handleId">> => Id,
        <<"publicHandle">> => expected_public_handle(Id),
        <<"resourceType">> => maps:get(<<"resourceType">>, HandleData, <<"Share">>),
        <<"resourceId">> => maps:get(<<"resourceId">>, HandleData),
        <<"metadata">> => maps:get(<<"metadata">>, HandleData),
        <<"timestamp">> => time:seconds_to_iso8601(ozt_mocks:get_frozen_time_seconds()),
        <<"creationTime">> => ozt_mocks:get_frozen_time_seconds()
    };
public_handle(gs, Id, HandleData) ->
    ?OK_MAP(#{
        <<"gri">> => gri:serialize(?GRI(od_handle, Id, instance, public)),
        <<"publicHandle">> => expected_public_handle(Id),
        <<"metadata">> => maps:get(<<"metadata">>, HandleData),
        <<"timestamp">> => time:seconds_to_iso8601(ozt_mocks:get_frozen_time_seconds())
    }).


-spec protected_harvester(interface(), od_harvester:id(), map(), aai:subject()) -> expectation().
protected_harvester(logic, _Id, HarvesterData, Creator) ->
    ?OK_MAP(#{
        <<"name">> => maps:get(<<"name">>, HarvesterData),
        <<"public">> => maps:get(<<"public">>, HarvesterData, false),
        <<"harvestingBackendEndpoint">> => maps:get(<<"harvestingBackendEndpoint">>, HarvesterData),
        <<"harvestingBackendType">> => maps:get(<<"harvestingBackendType">>, HarvesterData),
        <<"creationTime">> => ozt_mocks:get_frozen_time_seconds(),
        <<"creator">> => Creator
    });
protected_harvester(rest, Id, HarvesterData, Creator) ->
    #{
        <<"harvesterId">> => Id,
        <<"name">> => maps:get(<<"name">>, HarvesterData),
        <<"public">> => maps:get(<<"public">>, HarvesterData, false),
        <<"harvestingBackendEndpoint">> => maps:get(<<"harvestingBackendEndpoint">>, HarvesterData),
        <<"harvestingBackendType">> => atom_to_binary(maps:get(<<"harvestingBackendType">>, HarvesterData), utf8),
        <<"creationTime">> => ozt_mocks:get_frozen_time_seconds(),
        <<"creator">> => aai:subject_to_json(Creator)
    }.


-spec shared_or_public_harvester(interface(), od_harvester:id(), map()) -> expectation().
shared_or_public_harvester(logic, _Id, HarvesterData) ->
    ?OK_MAP(#{
        <<"name">> => maps:get(<<"name">>, HarvesterData)
    });
shared_or_public_harvester(rest, Id, HarvesterData) ->
    #{
        <<"harvesterId">> => Id,
        <<"name">> => maps:get(<<"name">>, HarvesterData)
    }.


-spec protected_atm_inventory(interface(), od_handle_service:id(), map(), aai:subject()) -> expectation().
protected_atm_inventory(logic, _Id, HServiceData, Creator) ->
    ?OK_MAP(#{
        <<"name">> => maps:get(<<"name">>, HServiceData),
        <<"creationTime">> => ozt_mocks:get_frozen_time_seconds(),
        <<"creator">> => Creator
    });
protected_atm_inventory(rest, Id, HServiceData, Creator) ->
    #{
        <<"atmInventoryId">> => Id,
        <<"name">> => maps:get(<<"name">>, HServiceData),
        <<"creationTime">> => ozt_mocks:get_frozen_time_seconds(),
        <<"creator">> => aai:subject_to_json(Creator)
    }.


-spec private_atm_lambda(interface(), od_atm_lambda:id(), map(), aai:subject()) -> expectation().
private_atm_lambda(logic, _Id, AtmLambdaData, Creator) ->
    ?OK_TERM(fun(AtmLambdaRecord) ->
        ?assertEqual(AtmLambdaRecord, #od_atm_lambda{
            name = maps:get(<<"name">>, AtmLambdaData),
            summary = maps:get(<<"summary">>, AtmLambdaData),
            description = maps:get(<<"description">>, AtmLambdaData),

            operation_spec = jsonable_record:from_json(maps:get(<<"operationSpec">>, AtmLambdaData), atm_lambda_operation_spec),
            argument_specs = [jsonable_record:from_json(S, atm_lambda_argument_spec) || S <- maps:get(<<"argumentSpecs">>, AtmLambdaData)],
            result_specs = [jsonable_record:from_json(S, atm_lambda_result_spec) || S <- maps:get(<<"resultSpecs">>, AtmLambdaData)],

            atm_inventories = [maps:get(<<"atmInventoryId">>, AtmLambdaData)],

            creation_time = ozt_mocks:get_frozen_time_seconds(),
            creator = Creator
        })
    end);
private_atm_lambda(rest, Id, AtmLambdaData, Creator) ->
    #{
        <<"atmLambdaId">> => Id,
        <<"name">> => maps:get(<<"name">>, AtmLambdaData),
        <<"summary">> => maps:get(<<"summary">>, AtmLambdaData),
        <<"description">> => maps:get(<<"description">>, AtmLambdaData),

        <<"operationSpec">> => maps:get(<<"operationSpec">>, AtmLambdaData),
        <<"argumentSpecs">> => maps:get(<<"argumentSpecs">>, AtmLambdaData),
        <<"resultSpecs">> => maps:get(<<"resultSpecs">>, AtmLambdaData),

        <<"creationTime">> => ozt_mocks:get_frozen_time_seconds(),
        <<"creator">> => aai:subject_to_json(Creator)
    };
private_atm_lambda(gs, Id, AtmLambdaData, _Creator) ->
    ?OK_MAP(#{
        <<"gri">> => gri:serialize(?GRI(od_atm_lambda, Id, instance, private)),
        <<"name">> => maps:get(<<"name">>, AtmLambdaData),
        <<"summary">> => maps:get(<<"summary">>, AtmLambdaData),
        <<"description">> => maps:get(<<"description">>, AtmLambdaData),

        <<"operationSpec">> => maps:get(<<"operationSpec">>, AtmLambdaData),
        <<"argumentSpecs">> => maps:get(<<"argumentSpecs">>, AtmLambdaData),
        <<"resultSpecs">> => maps:get(<<"resultSpecs">>, AtmLambdaData)
    }).

%%%===================================================================
%%% helpers
%%%===================================================================

%% @private
expected_public_share_url(ShareId) ->
    str_utils:format_bin("https://~s/share/~s", [ozt:get_domain(), ShareId]).


%% @private
expected_public_share_rest_url(ShareId) ->
    str_utils:format_bin("https://~s/api/v3/onezone/shares/~s/public", [ozt:get_domain(), ShareId]).


%% @private
expected_public_handle(HandleId) ->
    {ok, #document{value = #od_handle{
        handle_service = HandleServiceId,
        public_handle = PublicHandle
    }}} = ?assertMatch({ok, _}, ozt:rpc(od_handle, get, [HandleId])),
    {ok, #document{value = #od_handle_service{
        service_properties = #{<<"type">> := Type}
    }}} = ?assertMatch({ok, _}, ozt:rpc(od_handle_service, get, [HandleServiceId])),
    % DOI handles do not get a full URL (just the DOI id), but other handles do
    case Type of
        <<"DOI">> ->
            ok;
        _ ->
            % as mocked in oz_test_utils:mock_handle_proxy/1
            ?assertMatch(<<"http://hndl.service.example.com/", _/binary>>, PublicHandle)
    end,
    PublicHandle.


%% @private
expected_cluster_worker_version_info(?ONEZONE) ->
    #{
        <<"release">> => ozt:rpc(oz_worker, get_release_version, []),
        <<"build">> => ozt:rpc(oz_worker, get_build_version, []),
        <<"gui">> => element(2, {ok, _} = ozt:rpc(gui, package_hash, [ozt:get_env(ozw_gui_package_path)]))
    };
expected_cluster_worker_version_info(?ONEPROVIDER) ->
    #{
        <<"release">> => ?DEFAULT_RELEASE_VERSION,
        <<"build">> => ?DEFAULT_BUILD_VERSION,
        <<"gui">> => ?EMPTY_GUI_HASH
    }.


%% @private
expected_cluster_panel_version_info() ->
    #{
        <<"release">> => ?DEFAULT_RELEASE_VERSION,
        <<"build">> => ?DEFAULT_BUILD_VERSION,
        <<"gui">> => ?EMPTY_GUI_HASH
    }.


%% @private
expected_cluster_creation_time(?ONEZONE) ->
    {ok, #document{value = #od_cluster{
        creation_time = CreationTime
    }}} = ?assertMatch({ok, _}, ozt:rpc(od_cluster, get, [?ONEZONE_CLUSTER_ID])),
    CreationTime;
expected_cluster_creation_time(?ONEPROVIDER) ->
    ozt_mocks:get_frozen_time_seconds().

