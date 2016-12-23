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


-export([entity_type/0, create/4, get_entity/1, get_internal/4, get_external/2,
    update/3, delete/2]).
-export([exists/2, authorize/4, validate/2]).


entity_type() ->
    od_user.


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


get_entity(UserId) ->
    case od_user:get(UserId) of
        {ok, #document{value = Group}} ->
            {ok, Group};
        _ ->
            ?ERROR_NOT_FOUND
    end.


get_internal(?USER, _, _, _) ->
    ok.


get_external(_, list) ->
    {ok, UserDocs} = od_user:list(),
    {ok, [UserId || #document{key = UserId} <- UserDocs]};
get_external(?USER, _) ->
    ok.


% TODO update internal, external
update(UserId, entity, Data) when is_binary(UserId) ->
    {ok, _} = od_user:update(UserId, fun(User) ->
        {ok, User}
    end),
    ok;
update(UserId, oz_privileges, Data) ->
    Privileges = maps:get(<<"privileges">>, Data),
    Operation = maps:get(<<"operation">>, Data, set),
    entity_graph:update_oz_privileges(od_user, UserId, Operation, Privileges).


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


authorize(create, UserId, client_tokens, ?USER(UserId)) ->
    true;
authorize(create, UserId, join_group, ?USER(UserId)) ->
    true;
authorize(create, UserId, join_space, ?USER(UserId)) ->
    true;
authorize(update, _UserId, oz_privileges, ?USER(UserId)) ->
    auth_by_oz_privilege(UserId, set_privileges).


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
}.


auth_by_oz_privilege(_UserId, Privilege) ->
    {internal, fun(User) ->
        n_user_logic:has_eff_oz_privilege(User, Privilege)
    end}.
