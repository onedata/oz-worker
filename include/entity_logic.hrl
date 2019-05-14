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

-define(SELF_INTERMEDIARY, <<"self">>).

% Record expressing entity logic request client (REST and Graph Sync).
-record(client, {
    % root is allowed to do anything, it must be used with caution
    % (should not be used in any kind of external API!)
    type = nobody :: user | provider | root | nobody,
    id = <<"">> :: binary()
}).

% Record expressing entity logic request
-record(el_req, {
    client = #client{} :: entity_logic:client(),
    gri :: entity_logic:gri(),
    operation = create :: entity_logic:operation(),
    data = #{} :: entity_logic:data(),
    auth_hint = undefined :: undefined | entity_logic:auth_hint()
}).

% Convenience macros for concise code
-define(USER, #client{type = user}).
-define(USER(__Id), #client{type = user, id = __Id}).
-define(PROVIDER, #client{type = provider}).
-define(PROVIDER(__Id), #client{type = provider, id = __Id}).
-define(NOBODY, #client{type = nobody}).
-define(ROOT, #client{type = root}).

% Macros to strip results from entity_logic:create into simpler form.
-define(CREATE_RETURN_ID(__Expr),
    case __Expr of
        {error, _} = __Err ->
            __Err;
        ok ->
            throw(create_did_not_return_id);
        {ok, value, __Data} ->
            throw(create_did_not_return_id);
        {ok, resource, {#gri{id = __Id}, __Data}} ->
            {ok, __Id};
        {ok, resource, {#gri{id = __Id}, _AuthHint, __Data}} ->
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
        {ok, resource, {_GRI, __Data}} ->
            {ok, __Data};
        {ok, resource, {_GRI, _AuthHint, __Data}} ->
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

% Regexp to validate domain (domain, subdomain or IP)
% Domain consists of some number of parts delimited by single dot characters.
% Each part must start and end with an lowercase alphanum
% and may contain a hyphen '-'.
-define(DOMAIN_VALIDATION_REGEXP,
    <<"^(([a-z0-9]|[a-z0-9][a-z0-9\\-]*[a-z0-9])\\.)*([a-z0-9]|[a-z0-9][a-z0-9\\-]*[a-z0-9])$">>).

-define(MAX_DOMAIN_LENGTH, 253).

-define(SUBDOMAIN_VALIDATION_REGEXP,
    <<"^([a-z0-9]|[a-z0-9][a-z0-9\\-]*[a-z0-9])$">>).


-define(NAME_REQUIREMENTS_DESCRIPTION, <<
    "Name must be 2-50 characters long and composed only of UTF-8 letters, digits, brackets and underscores. "
    "Dashes, spaces and dots are allowed (but not at the beginning or the end)."
>>).
-define(NAME_FIRST_CHARS_ALLOWED, <<")(\\w_">>).
-define(NAME_MIDDLE_CHARS_ALLOWED, <<">>)(\\w_ .-">>).
-define(NAME_LAST_CHARS_ALLOWED, ?NAME_FIRST_CHARS_ALLOWED).
-define(NAME_MAXIMUM_LENGTH, 50).


-define(FULL_NAME_REQUIREMENTS_DESCRIPTION, <<
    "Full name must be 2-50 characters long and composed only of UTF-8 letters and digits. "
    "Dashes, spaces, dots, commas and apostrophes are allowed (but not at the beginning or the end)."
>>).
-define(FULL_NAME_FIRST_CHARS_ALLOWED, <<"\\pL\\pNd">>).
-define(FULL_NAME_MIDDLE_CHARS_ALLOWED, <<"\\pL\\pNd ',.-">>).
-define(FULL_NAME_LAST_CHARS_ALLOWED, <<"\\pL\\pNd.">>).
-define(FULL_NAME_MAXIMUM_LENGTH, 50).
-define(DEFAULT_FULL_NAME, <<"Unnamed User">>).


-define(USERNAME_REQUIREMENTS_DESCRIPTION, <<
    "Username must be 2-20 characters long and composed only of letters and digits. "
    "Dashes and underscores are allowed (but not at the beginning or the end). "
>>).
-define(USERNAME_FIRST_CHARS_ALLOWED, <<"a-z0-9A-Z">>).
-define(USERNAME_MIDDLE_CHARS_ALLOWED, <<"a-z0-9A-Z._-">>).
-define(USERNAME_LAST_CHARS_ALLOWED, ?USERNAME_FIRST_CHARS_ALLOWED).
-define(USERNAME_MAXIMUM_LENGTH, 20).


-define(PASSWORD_REQUIREMENTS_DESCRIPTION, <<
    "Password must be at least 8 characters long."
>>).
-define(PASSWORD_MIN_LENGTH, 8).


% Used when enable_automatic_first_space is set to true
-define(FIRST_SPACE_NAME, <<"Personal Space">>).

-endif.
