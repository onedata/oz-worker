%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Common definitions concerning entity logic.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(ENTITY_LOGIC_HRL).
-define(ENTITY_LOGIC_HRL, 1).

-include_lib("cluster_worker/include/graph_sync/graph_sync.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/validation.hrl").

-define(SELF_INTERMEDIARY, <<"self">>).

% Record expressing entity logic request
-record(el_req, {
    auth = ?NOBODY :: aai:auth(),
    gri :: entity_logic:gri(),
    operation = create :: entity_logic:operation(),
    data = #{} :: entity_logic:data(),
    auth_hint = undefined :: undefined | entity_logic:auth_hint(),
    % applicable for create/get requests - returns the revision of resource
    return_revision = false :: boolean()
}).

% Macros to strip results from entity_logic:create into simpler form.
-define(CREATE_RETURN_ID(__Expr),
    case __Expr of
        {error, _} = __Err ->
            __Err;
        ok ->
            throw(create_did_not_return_id);
        {ok, value, __Data} ->
            throw(create_did_not_return_id);
        {ok, resource, {#gri{id = __Id}, {__Data, __Rev}}} ->
            {ok, __Id};
        {ok, resource, {#gri{id = __Id}, _AuthHint, {__Data, __Rev}}} ->
            {ok, __Id}
    end
).

-define(CREATE_RETURN_DATA(__Expr),
    case __Expr of
        {error, _} = __Err ->
            __Err;
        ok ->
            throw(create_did_not_return_data);
        {ok, value, __Data} ->
            {ok, __Data};
        {ok, resource, {_GRI, {__Data, __Rev}}} ->
            {ok, __Data};
        {ok, resource, {_GRI, _AuthHint, {__Data, __Rev}}} ->
            {ok, __Data}
    end
).

-define(CREATE_RETURN_OK(__Expr),
    case __Expr of
        {error, _} = __Err ->
            __Err;
        ok ->
            ok;
        {ok, _, _} ->
            ok
    end
).


% Used when enable_automatic_first_space is set to true
-define(FIRST_SPACE_NAME, <<"Personal Space">>).
% Name of the provider's first named token (root token)
-define(PROVIDER_ROOT_TOKEN_NAME, <<"root token">>).
% Name generator for the legacy clients tokens (after migration)
-define(LEGACY_CLIENT_TOKEN_NAME(Number), <<"legacy token ", (integer_to_binary(Number))/binary>>).


%% @TODO VFS-5727 temporary solution
-define(INVITE_TOKEN_NAME(TokenType), <<
    (atom_to_binary(TokenType, utf8))/binary, " ",
    (binary:part(time_utils:epoch_to_iso8601(time_utils:cluster_time_seconds()), 0, 10))/binary, " ",
    (str_utils:rand_hex(3))/binary
>>).

%% @TODO VFS-5727 temporary solution
-define(ACCESS_TOKEN_NAME, <<
    "access token ",
    (binary:replace(
        time_utils:epoch_to_iso8601(time_utils:cluster_time_seconds()),
        <<$:>>, <<$.>>, [global]
    ))/binary, " ",
    (str_utils:rand_hex(3))/binary
>>).


%% @TODO VFS-5856 This is needed for providers in previous version, for which virtual
%% storage record supporting all their spaces will be created. This storage record
%% does not represent actual storage, it is provider that keeps knowledge of storages.
-define(STORAGE_DEFAULT_NAME, <<"default_storage">>).

-endif.
