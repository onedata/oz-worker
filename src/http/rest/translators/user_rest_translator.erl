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

-include("http/rest.hrl").
-include_lib("ctool/include/errors.hrl").

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
create_response(#gri{aspect = instance}, _, resource, {#gri{id = UserId}, _}) ->
    rest_translator:created_reply_with_location([<<"users">>, UserId]);

create_response(#gri{aspect = client_tokens}, _, resource, {_, {Token, _Rev}}) ->
    rest_translator:ok_body_reply(#{<<"token">> => Token});

create_response(#gri{aspect = {idp_access_token, _}}, _, value, {AccessToken, Expires}) ->
    rest_translator:ok_body_reply(#{
        <<"token">> => AccessToken,
        <<"ttl">> => Expires
    });

create_response(#gri{aspect = provider_registration_token}, _, value, Token) ->
    {ok, Serialized} = tokens:serialize(Token),
    rest_translator:ok_body_reply(#{<<"token">> => Serialized}).


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
        <<"basicAuthEnabled">> := BasicAuthEnabled,
        <<"blocked">> := Blocked,
        <<"fullName">> := FullName, <<"username">> := Username,
        <<"emails">> := Emails,
        <<"linkedAccounts">> := LinkedAccounts,
        <<"creationTime">> := CreationTime
    } = UserData,
    rest_translator:ok_body_reply(#{
        <<"basicAuthEnabled">> => BasicAuthEnabled,
        <<"blocked">> => Blocked,
        <<"userId">> => UserId,
        <<"fullName">> => FullName,
        <<"username">> => utils:undefined_to_null(Username),
        <<"emails">> => Emails,
        <<"linkedAccounts">> => LinkedAccounts,
        <<"creationTime">> => CreationTime,

        %% @TODO VFS-4506 deprecated fields, included for backward compatibility
        <<"name">> => FullName,
        <<"login">> => utils:undefined_to_null(Username),
        <<"alias">> => utils:undefined_to_null(Username),
        <<"emailList">> => Emails
    });

get_response(#gri{id = UserId, aspect = instance, scope = shared}, UserData) ->
    #{
        <<"fullName">> := FullName,
        <<"username">> := Username,
        <<"creationTime">> := CreationTime
    } = UserData,
    rest_translator:ok_body_reply(#{
        <<"userId">> => UserId,
        <<"fullName">> => FullName,
        <<"username">> => utils:undefined_to_null(Username),
        <<"creationTime">> => CreationTime,

        %% @TODO VFS-4506 deprecated fields, included for backward compatibility
        <<"name">> => FullName,
        <<"login">> => utils:undefined_to_null(Username),
        <<"alias">> => utils:undefined_to_null(Username)
    });

get_response(#gri{aspect = oz_privileges}, Privileges) ->
    rest_translator:ok_body_reply(#{<<"privileges">> => Privileges});

get_response(#gri{aspect = eff_oz_privileges}, Privileges) ->
    rest_translator:ok_body_reply(#{<<"privileges">> => Privileges});

get_response(#gri{aspect = {space_alias, _}}, SpaceAlias) ->
    rest_translator:ok_body_reply(#{<<"alias">> => SpaceAlias});

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
    rest_translator:ok_body_reply(#{<<"harvesters">> => Harvesters});


get_response(#gri{aspect = clusters}, Clusters) ->
    rest_translator:ok_body_reply(#{<<"clusters">> => Clusters});

get_response(#gri{aspect = eff_clusters}, Clusters) ->
    rest_translator:ok_body_reply(#{<<"clusters">> => Clusters}).
