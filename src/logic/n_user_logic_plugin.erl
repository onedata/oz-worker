%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc

%%% @end
%%%-------------------------------------------------------------------
-module(n_user_logic_plugin).
-author("Lukasz Opiola").
-behaviour(data_logic_plugin_behaviour).

-include("errors.hrl").
-include("tokens.hrl").
-include("entity_logic.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").


-export([entity_type/0, get_entity/1, create/4, get/4, update/3, delete/2]).
-export([exists/2, authorize/4, validate/2]).


entity_type() ->
    od_user.


get_entity(UserId) ->
    case od_user:get(UserId) of
        {ok, #document{value = Group}} ->
            {ok, Group};
        _ ->
            ?ERROR_NOT_FOUND
    end.


create(?USER(UserId), UserId, client_tokens, _Data) ->
    Token = auth_logic:gen_token(UserId),
    {ok, _} = od_user:update(UserId, fun(#od_user{client_tokens = Tokens}) ->
        {ok, #od_user{client_tokens = [Token | Tokens]}}
    end),
    {ok, Token};

create(?USER(UserId), UserId, join_group, Data) ->
    Macaroon = maps:get(<<"token">>, Data),
    {ok, {od_group, GroupId}} = token_logic:consume(Macaroon),
    entity_graph:add_relation(
        od_user, UserId, od_group, GroupId, privileges:group_user()
    ),
    {ok, GroupId};

create(?USER(UserId), UserId, join_space, Data) ->
    Macaroon = maps:get(<<"token">>, Data),
    {ok, {od_space, SpaceId}} = token_logic:consume(Macaroon),
    entity_graph:add_relation(
        od_user, UserId, od_space, SpaceId, privileges:space_user()
    ),
    {ok, SpaceId}.


get(_, undefined, undefined, list) ->
    {ok, UserDocs} = od_user:list(),
    {ok, [UserId || #document{key = UserId} <- UserDocs]};

get(_, _UserId, #od_user{default_space = DefaultSpace}, default_space) ->
    {ok, DefaultSpace};
get(_, _UserId, #od_user{space_aliases = SpaceAliases}, {space_alias, SpaceId}) ->
    {ok, maps:get(SpaceId, SpaceAliases)};
get(_, _UserId, #od_user{default_provider = DefaultProvider}, default_provider) ->
    {ok, DefaultProvider};
get(_, _UserId, #od_user{client_tokens = ClientTokens}, client_tokens) ->
    {ok, ClientTokens};

get(_, _UserId, #od_user{groups = Groups}, groups) ->
    {ok, Groups};
get(_, _UserId, #od_user{eff_groups = Groups}, eff_groups) ->
    {ok, Groups};
get(_, _UserId, #od_user{}, {group, GroupId}) ->
    ?assert_success(n_group_logic_plugin:get_entity(GroupId));
get(_, _UserId, #od_user{}, {eff_group, GroupId}) ->
    ?assert_success(n_group_logic_plugin:get_entity(GroupId));

get(_, _UserId, #od_user{spaces = Spaces}, spaces) ->
    {ok, Spaces};
get(_, _UserId, #od_user{eff_spaces = Spaces}, eff_spaces) ->
    {ok, Spaces};
get(_, _UserId, #od_user{}, {space, SpaceId}) ->
    ?assert_success(n_space_logic_plugin:get_entity(SpaceId));
get(_, _UserId, #od_user{}, {eff_space, SpaceId}) ->
    ?assert_success(n_space_logic_plugin:get_entity(SpaceId));

get(_, _UserId, #od_user{eff_providers = Providers}, eff_providers) ->
    {ok, Providers};
get(_, _UserId, #od_user{}, {eff_provider, ProviderId}) ->
    ?assert_success(n_provider_logic_plugin:get_entity(ProviderId));

get(_, _UserId, #od_user{handle_services = HandleServices}, handle_services) ->
    {ok, HandleServices};
get(_, _UserId, #od_user{eff_handle_services = HandleServices}, eff_handle_services) ->
    {ok, HandleServices};
get(_, _UserId, #od_user{}, {handle_service, HServiceId}) ->
    ?assert_success(n_handle_service_logic_plugin:get_entity(HServiceId));
get(_, _UserId, #od_user{}, {eff_handle_service, HServiceId}) ->
    ?assert_success(n_handle_service_logic_plugin:get_entity(HServiceId));

get(_, _UserId, #od_user{handles = Handles}, handles) ->
    {ok, Handles};
get(_, _UserId, #od_user{eff_handles = Handles}, eff_handles) ->
    {ok, Handles};
get(_, _UserId, #od_user{}, {handle, HandleId}) ->
    ?assert_success(n_handle_logic_plugin:get_entity(HandleId));
get(_, _UserId, #od_user{}, {eff_handle, HandleId}) ->
    ?assert_success(n_handle_logic_plugin:get_entity(HandleId)).


update(UserId, entity, Data) when is_binary(UserId) ->
    UserUpdateFun = fun(#od_user{name = OldName, alias = OldAlias} = User) ->
        {ok, User#od_user{
            name = maps:get(<<"name">>, Data, OldName),
            alias = maps:get(<<"alias">>, Data, OldAlias)
        }}
    end,
    % If alias is specified, run update in synchronized block so no two
    % identical aliases can be set
    case maps:get(<<"alias">>, Data, undefined) of
        undefined ->
            {ok, _} = od_user:update(UserId, UserUpdateFun),
            ok;
        Alias ->
            critical_section:run({alias, Alias}, fun() ->
                % Check if this alias is occupied
                case od_user:get_by_criterion({alias, Alias}) of
                    {ok, #document{key = UserId}} ->
                        % DB returned the same user, so the alias was modified
                        % but is identical, don't report errors.
                        {ok, _} = od_user:update(UserId, UserUpdateFun),
                        ok;
                    {ok, #document{}} ->
                        % Alias is occupied by another user
                        ?ERROR_ALIAS_OCCUPIED;
                    _ ->
                        % Alias is not occupied, update user doc
                        {ok, _} = od_user:update(UserId, UserUpdateFun),
                        ok
                end
            end)
    end;
update(UserId, oz_privileges, Data) ->
    Privileges = maps:get(<<"privileges">>, Data),
    Operation = maps:get(<<"operation">>, Data, set),
    entity_graph:update_oz_privileges(od_user, UserId, Operation, Privileges);

update(UserId, default_space, Data) ->
    SpaceId = maps:get(<<"spaceId">>, Data),
    {ok, _} = od_user:update(UserId, #{default_space => SpaceId}),
    ok;

update(UserId, {space_alias, SpaceId}, Data) ->
    Alias = maps:get(<<"alias">>, Data),
    {ok, _} = od_user:update(UserId, fun(#od_user{space_aliases = Aliases} = User) ->
        {ok, User#od_user{space_aliases = maps:put(SpaceId, Alias, Aliases)}}
    end),
    ok;

update(UserId, default_provider, Data) ->
    ProviderId = maps:get(<<"providerId">>, Data),
    {ok, _} = od_user:update(UserId, #{default_provider => ProviderId}),
    ok.


delete(UserId, entity) when is_binary(UserId) ->
    entity_graph:delete_with_relations(od_user, UserId).


exists(undefined, _) ->
    true;
exists(UserId, _) when is_binary(UserId) ->
    {internal, fun(#od_user{}) ->
        % If the user with UserId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end}.


authorize(_, UserId, _, ?USER(UserId)) when is_binary(UserId) ->
    % User can create/get/update/delete any information about himself
    true;

% Other (admin) users privileges
authorize(update, _UserId, oz_privileges, ?USER(UserId)) ->
    auth_by_oz_privilege(UserId, ?OZ_SET_PRIVILEGES).



validate(create, join_group) -> #{
    required => #{
        <<"token">> => {token, ?GROUP_INVITE_USER_TOKEN}
    }
};
validate(create, join_space) -> #{
    required => #{
        <<"token">> => {token, ?SPACE_INVITE_USER_TOKEN}
    }
};
validate(update, oz_privileges) -> #{
    required => #{
        <<"privileges">> => {list_of_atoms, privileges:oz_privileges()}
    },
    optional => #{
        <<"operation">> => {atom, [set, grant, revoke]}
    }
};
validate(update, default_space) -> #{
    required => #{
        <<"spaceId">> => {binary, {exists, fun(Value) ->
            n_space_logic:exists(Value)
        end}}
    }
};
validate(update, {space_alias, _SpaceId}) -> #{
    required => #{
        <<"alias">> => {binary, {custom, fun(Value) ->
            RegExpValidation = (match =:= re:run(
                Value, ?ALIAS_VALIDATION_REGEXP, [{capture, none}]
            )),
            case {Value, RegExpValidation} of
                {?EMPTY_ALIAS, _} -> false;
                {<<?NO_ALIAS_UUID_PREFIX, _/binary>>, _} -> false;
                {_, false} -> false;
                {_, true} -> true
            end
        end, ?ERROR_BAD_VALUE_ALIAS(<<"alias">>)}}
    }
}.


auth_by_oz_privilege(_UserId, Privilege) ->
    {internal, fun(User) ->
        n_user_logic:has_eff_oz_privilege(User, Privilege)
    end}.
