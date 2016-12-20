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

-include("entity_logic.hrl").
-include("errors.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").


-export([create/4, get_entity/1, get_internal/4, get_external/2, update/3,
    delete/1]).
-export([exists/2, authorize/5, validate/2]).


create(_, _, _, _) ->
    error.


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


delete(UserId) when is_binary(UserId) ->
    ok = od_user:delete(UserId).


exists(undefined, entity) ->
    true;
exists(undefined, list) ->
    true;
exists(UserId, _) when is_binary(UserId) ->
    {internal, fun(#od_user{}) ->
        % If the user with UserId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end}.


authorize(update, UserId, oz_privileges, ?USER(UserId), _) ->
    auth_by_oz_privilege(UserId, set_privileges).


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
