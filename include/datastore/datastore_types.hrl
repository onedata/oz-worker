%%%-------------------------------------------------------------------
%%% @author Rafal Slota
%%% @copyright (C): 2013 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Datastore types definitions
%%% @end
%%%-------------------------------------------------------------------

-ifndef(DATASTORE_TYPES_HRL).
-define(DATASTORE_TYPES_HRL, 1).

-include("datastore/gr_datastore_models_def.hrl").

%% Wrapper for all models' records
%% todo: remove this duplicate definition and resolve issues with some_record redefined on proper includes
-record(document, {
  key :: datastore:ext_key(),
  rev :: term(),
  value :: datastore:value(),
  links :: term()
}).

-type model_id() :: binary().
-type db_doc() :: #document{}.

-type url() :: string().
-type privileges() :: none | invite | admin.

-type user_id() :: model_id().
-type user_info() :: #onedata_user{}.
-type user_doc() :: #document{value :: #onedata_user{}}.

-type provider_id() :: model_id().
-type provider_info() :: #provider{}.
-type provider_doc() :: #document{value :: #provider{}}.

-type group_id() :: model_id().
-type group_info() :: #user_group{}.
-type group_doc() :: #document{value :: #user_group{}}.

-type space_id() :: model_id().
-type space_info() :: #space{}.
-type space_doc() :: #document{value :: #space{}}.

-type token_id() :: model_id().
-type token_info() :: #token{}.
-type token_doc() :: #document{value :: #token{}}.

-type auth_id() :: model_id().
-type auth_info() :: #onedata_auth{}.
-type auth_doc() :: #document{value :: #onedata_auth{}}.

-endif.
