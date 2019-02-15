%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module handles translation of entity logic results concerning
%%% user entities into REST responses.
%%% @end
%%%-------------------------------------------------------------------
-module(user_rest_translator).
-behaviour(rest_translator_behaviour).
-author("Lukasz Opiola").

-include("rest.hrl").
-include_lib("ctool/include/api_errors.hrl").

-export([create_response/4, get_response/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link rest_translator_behaviour} callback create_response/4.
%% @end
%%--------------------------------------------------------------------
-spec create_response(entity_logic:gri(), entity_logic:auth_hint(),
    entity_logic:data_format(), Result :: term() | {entity_logic:gri(), term()} |
    {entity_logic:gri(), entity_logic:auth_hint(), term()}) -> #rest_resp{}.
create_response(#gri{aspect = authorize}, _, value, DischargeMacaroon) ->
    rest_translator:ok_body_reply({binary, DischargeMacaroon});

create_response(#gri{aspect = client_tokens}, _, resource, {_, Token}) ->
    rest_translator:ok_body_reply(#{<<"token">> => Token});

create_response(#gri{aspect = {idp_access_token, _}}, _, value, {AccessToken, Expires}) ->
    rest_translator:ok_body_reply(#{
        <<"token">> => AccessToken,
        <<"ttl">> => Expires
    }).


%%--------------------------------------------------------------------
%% @doc
%% {@link rest_translator_behaviour} callback get_response/2.
%% @end
%%--------------------------------------------------------------------
-spec get_response(entity_logic:gri(), Resource :: term()) -> #rest_resp{}.
get_response(#gri{id = undefined, aspect = list}, Users) ->
    rest_translator:ok_body_reply(#{<<"users">> => Users});

get_response(#gri{id = UserId, aspect = instance, scope = protected}, UserData) ->
    #{
        <<"name">> := Name, <<"alias">> := Alias,
        <<"emails">> := Emails,
        <<"linkedAccounts">> := LinkedAccounts
    } = UserData,
    rest_translator:ok_body_reply(#{
        <<"userId">> => UserId,
        <<"name">> => Name,
        <<"alias">> => gs_protocol:undefined_to_null(Alias),
        <<"emails">> => Emails,
        <<"linkedAccounts">> => LinkedAccounts,

        % TODO VFS-4506 deprecated fields, included for backward compatibility
        <<"login">> => gs_protocol:undefined_to_null(Alias),
        <<"emailList">> => Emails
    });

get_response(#gri{id = UserId, aspect = instance, scope = shared}, UserData) ->
    #{
        <<"name">> := Name, <<"alias">> := Alias
    } = UserData,
    rest_translator:ok_body_reply(#{
        <<"userId">> => UserId,
        <<"name">> => Name,
        <<"alias">> => gs_protocol:undefined_to_null(Alias),
        % TODO VFS-4506 deprecated fields, included for backward compatibility
        <<"login">> => gs_protocol:undefined_to_null(Alias)
    });

get_response(#gri{aspect = oz_privileges}, Privileges) ->
    rest_translator:ok_body_reply(#{<<"privileges">> => Privileges});

get_response(#gri{aspect = eff_oz_privileges}, Privileges) ->
    rest_translator:ok_body_reply(#{<<"privileges">> => Privileges});

get_response(#gri{aspect = default_space}, DefaultSpace) ->
    rest_translator:ok_body_reply(#{<<"spaceId">> => DefaultSpace});

get_response(#gri{aspect = {space_alias, _}}, SpaceAlias) ->
    rest_translator:ok_body_reply(#{<<"alias">> => SpaceAlias});

get_response(#gri{aspect = default_provider}, DefaultProvider) ->
    rest_translator:ok_body_reply(#{<<"providerId">> => DefaultProvider});

get_response(#gri{aspect = client_tokens}, Tokens) ->
    rest_translator:ok_body_reply(#{<<"tokens">> => Tokens});

get_response(#gri{aspect = groups}, Groups) ->
    rest_translator:ok_body_reply(#{<<"groups">> => Groups});

get_response(#gri{aspect = eff_groups}, Groups) ->
    rest_translator:ok_body_reply(#{<<"groups">> => Groups});


get_response(#gri{aspect = spaces}, Spaces) ->
    rest_translator:ok_body_reply(#{<<"spaces">> => Spaces});

get_response(#gri{aspect = eff_spaces}, Spaces) ->
    rest_translator:ok_body_reply(#{<<"spaces">> => Spaces});


get_response(#gri{aspect = eff_providers}, Providers) ->
    rest_translator:ok_body_reply(#{<<"providers">> => Providers});


get_response(#gri{aspect = handle_services}, HServices) ->
    rest_translator:ok_body_reply(#{<<"handle_services">> => HServices});

get_response(#gri{aspect = eff_handle_services}, HServices) ->
    rest_translator:ok_body_reply(#{<<"handle_services">> => HServices});


get_response(#gri{aspect = handles}, Handles) ->
    rest_translator:ok_body_reply(#{<<"handles">> => Handles});

get_response(#gri{aspect = eff_handles}, Handles) ->
    rest_translator:ok_body_reply(#{<<"handles">> => Handles});

get_response(#gri{aspect = harvesters}, Harvesters) ->
    rest_translator:ok_body_reply(#{<<"harvesters">> => Harvesters});

get_response(#gri{aspect = eff_harvesters}, Harvesters) ->
    rest_translator:ok_body_reply(#{<<"harvesters">> => Harvesters}).

