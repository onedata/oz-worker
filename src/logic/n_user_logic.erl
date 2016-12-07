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
-module(n_user_logic).
-author("Lukasz Opiola").
-behaviour(data_logic_behaviour).

-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-define(PLUGIN, n_user_logic_plugin).

-export([
    create/1
]).
-export([
    get/2
]).
-export([
    modify_oz_privileges/4, modify_oz_privileges/3
%%    join_as_user/2,
%%    join_as_group/3
]).
-export([
    update/3
]).
-export([
    delete/2
]).
-export([
    exists/1,
    has_eff_oz_privilege/2
]).


create(UserInfo) ->
    od_user:create(#document{value = UserInfo}).


get(Issuer, UserId) ->
    n_entity_logic:get(Issuer, ?PLUGIN, UserId, entity).


modify_oz_privileges(Issuer, UserId, Operation, Privs) when is_list(Privs) ->
    modify_oz_privileges(Issuer, UserId, #{
        <<"operation">> => Operation,
        <<"privileges">> => Privs
    }).
modify_oz_privileges(Issuer, UserId, Data) ->
    n_entity_logic:update(Issuer, ?PLUGIN, UserId, oz_privileges, Data).


update(Issuer, UserId, NewName) when is_binary(NewName) ->
    update(Issuer, UserId, #{<<"name">> => NewName});
update(Issuer, UserId, Data) ->
    n_entity_logic:update(Issuer, ?PLUGIN, UserId, entity, Data).


delete(Issuer, UserId) ->
    n_entity_logic:delete(Issuer, ?PLUGIN, UserId, entity).


%%--------------------------------------------------------------------
%% @doc
%% Returns whether a user exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(UserId :: od_user:id()) -> boolean().
exists(UserId) ->
    od_user:exists(UserId).


has_eff_oz_privilege(UserId, Privilege) when is_binary(UserId) ->
    {ok, #document{value = User}} = od_user:get(UserId),
    has_eff_oz_privilege(User, Privilege);
has_eff_oz_privilege(#od_user{eff_oz_privileges = UserPrivileges}, Privilege) ->
    ?emergency("~p ? ~p", [Privilege, UserPrivileges]),
    lists:member(Privilege, UserPrivileges).
