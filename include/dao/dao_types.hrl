%% ===================================================================
%% @author Rafal Slota
%% @copyright (C): 2013 ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: DAO types definitions
%% @end
%% ===================================================================

-ifndef(DAO_TYPES_HRL).
-define(DAO_TYPES_HRL, 1).
-include("dao/dao.hrl").
-include("dao/dao_users.hrl").
-include("dao/dao_providers.hrl").
-include("dao/dao_groups.hrl").
-include("dao/dao_spaces.hrl").
-include("dao/dao_tokens.hrl").


-type uuid() :: string(). %% Pattern: "^[0-9a-f]+$"
-type veil_doc() :: #veil_document{}.

-type url() :: string().
-type privileges() :: none | invite | admin.

-type user_id() :: uuid().
-type user_info() :: #user{}.
-type user_doc() :: #veil_document{record :: #user{}}.

-type provider_id() :: uuid().
-type provider_info() :: #provider{}.
-type provider_doc() :: #veil_document{record :: #provider{}}.

-type group_id() :: uuid().
-type group_info() :: #user_group{}.
-type group_doc() :: #veil_document{record :: #user_group{}}.

-type space_id() :: uuid().
-type space_info() :: #space{}.
-type space_doc() :: #veil_document{record :: #space{}}.

-type token_id() :: uuid().
-type token_info() :: #token{}.
-type token_doc() :: #veil_document{record :: #token{}}.

-endif.
