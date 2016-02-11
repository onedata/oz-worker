%%%-------------------------------------------------------------------
%%% @author Rafal Slota
%%% @copyright (C): 2013 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc DAO types definitions
%%% @end
%%%-------------------------------------------------------------------

-ifndef(DAO_TYPES_HRL).
-define(DAO_TYPES_HRL, 1).

-include("dao/dao_auth.hrl").
-include("dao/dao_users.hrl").
-include("dao/dao_providers.hrl").
-include("dao/dao_groups.hrl").
-include("dao/dao_spaces.hrl").
-include("dao/dao_tokens.hrl").
-include_lib("dao/include/common.hrl").


-type uuid() :: string(). %% Pattern: "^[0-9a-f]+$"
-type db_doc() :: #db_document{}.

-type url() :: string().
-type privileges() :: none | invite | admin.

-type user_id() :: uuid().
-type user_info() :: #user{}.
-type user_doc() :: #db_document{record :: #user{}}.

-type provider_id() :: uuid().
-type provider_info() :: #provider{}.
-type provider_doc() :: #db_document{record :: #provider{}}.

-type group_id() :: uuid().
-type group_info() :: #user_group{}.
-type group_doc() :: #db_document{record :: #user_group{}}.

-type space_id() :: uuid().
-type space_info() :: #space{}.
-type space_doc() :: #db_document{record :: #space{}}.

-type token_id() :: uuid().
-type token_info() :: #token{}.
-type token_doc() :: #db_document{record :: #token{}}.

-type auth_id() :: uuid().
-type auth_info() :: #auth{}.
-type auth_doc() :: #db_document{record :: #auth{}}.

-endif.