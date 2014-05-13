%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: The module implementing the business logic for registry's users.
%% This module serves as a buffer between the database and the REST API.
%% @end
%% ===================================================================
-module(space_logic).
-author("Konrad Zemek").

%% API

-export([can_provider_create/1, can_user_modify/3, can_client_view/2, can_user_invite/2, can_user_add_providers/2, can_user_delete/2, create/2, modify/2, new_user_invite_token/1, new_group_invite_token/2, new_support_token/0, get_data/2, delete/1]).

can_provider_create(Token) ->
    true.

can_user_modify(SpaceId, UserId, Data) ->
    true.

can_client_view(SpaceId, Client) ->
    true.

can_user_invite(SpaceId, UserId) ->
    true.

can_user_add_providers(SpaceId, UserId) ->
    true.

can_user_delete(SpaceId, UserId) ->
    true.

create(Client, Data) ->
    {ok, <<"spaceid">>}.

modify(SpaceId, Data) ->
    ok.

new_user_invite_token(UserId) ->
    <<"spaceuserinvitetoken">>.

new_group_invite_token(GroupId, UserId) ->
    <<"spacegroupinvitetoken">>.

new_support_token() ->
    <<"spacesupporttoken">>.

get_data(SpaceId, Client) ->
    {ok, [{<<"data">>, 1}]}.

delete(SpaceId) ->
    ok.
