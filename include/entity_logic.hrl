%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
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

%% A description of request client (REST and subscriptions).
-record(client, {
    % root is allowed to do anything, it must be used with caution
    % (should not be used in any kind of API!)
    type = nobody :: user | provider | root | nobody,
    id = <<"">> :: binary()
}).
% Convenience macros for concise code
-define(USER, #client{type = user}).
-define(USER(__Id), #client{type = user, id = __Id}).
-define(PROVIDER, #client{type = provider}).
-define(PROVIDER(__Id), #client{type = provider, id = __Id}).
-define(NOBODY, #client{type = nobody}).
-define(ROOT, #client{type = root}).

% Macro used in entity logic modules to make sure that result of a call
% returns success, in other case it throws an error that will be caught by
% entity_logic and properly handled.
-define(throw_on_failure(__Result), case __Result of
    {error, __Reason} ->
        throw({error, __Reason});
    __Success ->
        __Success
end).

% Definitions concerning aliases
% Value in DB meaning that alias is not set.
-define(EMPTY_ALIAS, <<"">>).

% Regexp to validate aliases - at least 5 alphanumeric chars
-define(ALIAS_VALIDATION_REGEXP, <<"^[a-z0-9]{5,}$">>).

% String that will be put in front of uuid when a user does not have
% an alias set. Aliases are not allowed to start with this string.
-define(NO_ALIAS_UUID_PREFIX, "uuid_").

-endif.