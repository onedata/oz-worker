%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Definitions concerning tokens in onezone.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(TOKENS_HRL).
-define(TOKENS_HRL, 1).

% Token types
-define(GROUP_INVITE_USER_TOKEN, group_invite_user_token).
-define(GROUP_INVITE_GROUP_TOKEN, group_invite_group_token).
-define(SPACE_INVITE_USER_TOKEN, space_invite_user_token).
-define(SPACE_INVITE_GROUP_TOKEN, space_invite_group_token).
-define(SPACE_SUPPORT_TOKEN, space_support_token).
-define(PROVIDER_REGISTRATION_TOKEN, provider_registration_token).

-endif.
