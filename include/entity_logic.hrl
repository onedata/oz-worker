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
-define(assert_success(__Result), case __Result of
    {error, __Reason} ->
        throw({error, __Reason});
    __Success ->
        __Success
end).

% Conversion to human readable strings
-define(ENTITY_TO_READABLE(__EntityType, __EntityId), begin
    __TypeString = case __EntityType of
        od_user -> "user";
        od_group -> "group";
        od_space -> "space";
        od_share -> "share";
        od_provider -> "provider";
        od_handle_service -> "handle_service";
        od_handle -> "handle"
    end,
    str_utils:format_bin("~s:~s", [__TypeString, __EntityId])
end).

-define(CLIENT_TO_READABLE(__Client), begin
    case __Client of
        ?NOBODY -> "nobody (unauthenticated user)";
        ?ROOT -> "root";
        ?USER(__UId) -> ?ENTITY_TO_READABLE(od_user, __UId);
        ?PROVIDER(__PId) -> ?ENTITY_TO_READABLE(od_provider, __PId)
    end
end).

-endif.
