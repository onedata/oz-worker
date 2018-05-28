%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module encapsulates all provider logic functionalities.
%%% In most cases, it is a wrapper for entity_logic functions.
%%% @end
%%%-------------------------------------------------------------------
-module(provider_logic).
-author("Lukasz Opiola").

-include("entity_logic.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("hackney/include/hackney_lib.hrl").

-define(PLUGIN, provider_logic_plugin).

-export([
    create_provider_registration_token/1,
    create/4, create/6, create/2, create_dev/2
]).
-export([
    get/2,
    get_protected_data/2,
    list/1
]).
-export([
    update/3
]).
-export([
    delete/2
]).
-export([
    get_eff_users/2, get_eff_user/3,
    get_eff_groups/2, get_eff_group/3,
    get_spaces/2, get_space/3,
    support_space/4, support_space/3,
    update_support_size/4,
    revoke_support/3
]).
-export([
    update_domain_config/3,
    get_domain_config/2,
    set_dns_txt_record/4,
    set_dns_txt_record/5,
    remove_dns_txt_record/3
]).
-export([
    check_my_ports/2,
    get_current_time/1,
    verify_provider_identity/2, verify_provider_identity/3
]).
-export([
    exists/1,
    has_eff_user/2,
    has_eff_group/2,
    supports_space/2
]).
-export([
    get_url/1,
    choose_provider_for_user/1
]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a new provider document in database based on Name,
%% Domain and AdminEmail.
%% @end
%%--------------------------------------------------------------------
-spec create(Client :: entity_logic:client(), Name :: binary(),
    Domain :: binary(), AdminEmail :: binary()) ->
    {ok, od_provider:id()} | {error, term()}.
create(Client, Name, Domain, AdminEmail) ->
    create(Client, #{
        <<"name">> => Name,
        <<"domain">> => Domain,
        <<"subdomainDelegation">> => false,
        <<"adminEmail">> => AdminEmail
    }).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new provider document in database based on Name,
%% Domain, AdminEmail, Latitude and Longitude.
%% @end
%%--------------------------------------------------------------------
-spec create(Client :: entity_logic:client(), Name :: binary(),
    Domain :: binary(), AdminEmail :: binary(), Latitude :: float(),
    Longitude :: float()) -> {ok, od_provider:id()} | {error, term()}.
create(Client, Name, Domain, AdminEmail, Latitude, Longitude) ->
    create(Client, #{
        <<"name">> => Name,
        <<"domain">> => Domain,
        <<"subdomainDelegation">> => false,
        <<"adminEmail">> => AdminEmail,
        <<"latitude">> => Latitude,
        <<"longitude">> => Longitude
    }).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new provider document in database. Name, URLs, Domain and
%% CSR (Certificate Signing Request) are provided in a
%% proper Data object, Latitude and Longitude are optional.
%% @end
%%--------------------------------------------------------------------
-spec create(Client :: entity_logic:client(), Data :: #{}) ->
    {ok, od_provider:id()} | {error, term()}.
create(Client, Data) ->
    Res = entity_logic:handle(#el_req{
        operation = create,
        client = Client,
        gri = #gri{type = od_provider, id = undefined, aspect = instance},
        data = Data
    }),
    case Res of
        Error = {error, _} ->
            Error;
        {ok, {fetched, #gri{id = ProviderId}, {_, Certificate}}} ->
            {ok, {ProviderId, Certificate}}
    end.


%%--------------------------------------------------------------------
%% @doc
%% TODO This is a developer functionality and should be removed when
%% TODO VFS-2550 is ready.
%% Creates a new provider document in database. UUID, Name,
%% Domain and CSR (Certificate Signing Request) are provided in a
%% proper Data object, Latitude and Longitude are optional.
%% @end
%%--------------------------------------------------------------------
-spec create_dev(Client :: entity_logic:client(), Data :: #{}) ->
    {ok, od_provider:id()} | {error, term()}.
create_dev(Client, Data) ->
    Res = entity_logic:handle(#el_req{
        operation = create,
        client = Client,
        gri = #gri{type = od_provider, id = undefined, aspect = instance_dev},
        data = Data
    }),
    case Res of
        Error = {error, _} ->
            Error;
        {ok, {fetched, #gri{id = ProviderId}, {_, Certificate}}} ->
            {ok, {ProviderId, Certificate}}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a provider record from database.
%% @end
%%--------------------------------------------------------------------
-spec get(Client :: entity_logic:client(), ProviderId :: od_provider:id()) ->
    {ok, #od_provider{}} | {error, term()}.
get(Client, ProviderId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_provider, id = ProviderId, aspect = instance}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves protected provider data from database.
%% @end
%%--------------------------------------------------------------------
-spec get_protected_data(Client :: entity_logic:client(), ProviderId :: od_provider:id()) ->
    {ok, maps:map()} | {error, term()}.
get_protected_data(Client, ProviderId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_provider, id = ProviderId, aspect = instance, scope = protected}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Lists all providers (their ids) in database.
%% @end
%%--------------------------------------------------------------------
-spec list(Client :: entity_logic:client()) ->
    {ok, [od_provider:id()]} | {error, term()}.
list(Client) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_provider, id = undefined, aspect = list}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates information of given provider.
%% Supports updating Name, Latitude and Longitude.
%% @end
%%--------------------------------------------------------------------
-spec update(Client :: entity_logic:client(), ProviderId :: od_provider:id(),
    Data :: #{}) -> ok | {error, term()}.
update(Client, ProviderId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        client = Client,
        gri = #gri{type = od_provider, id = ProviderId, aspect = instance},
        data = Data
    }).


%%--------------------------------------------------------------------
%% @doc
%% Deletes given provider from database.
%% @end
%%--------------------------------------------------------------------
-spec delete(Client :: entity_logic:client(), ProviderId :: od_provider:id()) ->
    ok | {error, term()}.
delete(Client, ProviderId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        client = Client,
        gri = #gri{type = od_provider, id = ProviderId, aspect = instance}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Creates a provider registration token,
%% which can be used by any provider to join Onezone.
%% @end
%%--------------------------------------------------------------------
-spec create_provider_registration_token(Client :: entity_logic:client()) ->
    {ok, macaroon:macaroon()} | {error, term()}.
create_provider_registration_token(Client) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        client = Client,
        gri = #gri{type = od_provider, aspect = provider_registration_token}
    })).


%%--------------------------------------------------------------------
%% @doc
%% Supports a space based on support_space_token and support size.
%% @end
%%--------------------------------------------------------------------
-spec support_space(Client :: entity_logic:client(), ProviderId :: od_provider:id(),
    Token :: token:id() | macaroon:macaroon(), SupportSize :: integer()) ->
    {ok, od_space:id()} | {error, term()}.
support_space(Client, ProviderId, Token, SupportSize) ->
    support_space(Client, ProviderId, #{
        <<"token">> => Token, <<"size">> => SupportSize
    }).


%%--------------------------------------------------------------------
%% @doc
%% Supports a space. Token (support_space_token) and SupportSize
%% are provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec support_space(Client :: entity_logic:client(), ProviderId :: od_provider:id(),
    Data :: #{}) -> {ok, od_space:id()} | {error, term()}.
support_space(Client, ProviderId, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        client = Client,
        gri = #gri{type = od_provider, id = ProviderId, aspect = support},
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Update data related to domain config and subdomain delegation.
%% @end
%%--------------------------------------------------------------------
-spec update_domain_config(Client :: entity_logic:client(),
    ProviderId :: od_provider:id(), Data :: #{}) -> ok | {error, term()}.
update_domain_config(Client, ProviderId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        client = Client,
        gri = #gri{type = od_provider, id = ProviderId, aspect = domain_config},
        data = Data}).

%%--------------------------------------------------------------------
%% @doc
%% Sets txt record for provider's subdomain with default TTL.
%% @end
%%--------------------------------------------------------------------
-spec set_dns_txt_record(Client :: entity_logic:client(),
    ProviderId :: od_provider:id(), Name :: binary(), Content :: binary()) ->
    ok | {error, term()}.
set_dns_txt_record(Client, ProviderId, Name, Content) ->
    set_dns_txt_record(Client, ProviderId, Name, Content, undefined).

%%--------------------------------------------------------------------
%% @doc
%% Sets txt record for provider's subdomain with given TTL.
%% @end
%%--------------------------------------------------------------------
-spec set_dns_txt_record(Client :: entity_logic:client(),
    ProviderId :: od_provider:id(), Name :: binary(), Content :: binary(),
    TTL :: dns_state:ttl()) ->
    ok | {error, term()}.
set_dns_txt_record(Client, ProviderId, Name, Content, TTL) ->
    entity_logic:handle(#el_req{
        operation = create,
        client = Client,
        gri = #gri{type = od_provider, id = ProviderId, aspect = {dns_txt_record, Name}},
        data = #{<<"content">> => Content, <<"ttl">> => TTL}
    }).

%%--------------------------------------------------------------------
%% @doc
%% Remove txt record for provider's subdomain
%% @end
%%--------------------------------------------------------------------
-spec remove_dns_txt_record(Client :: entity_logic:client(),
    ProviderId :: od_provider:id(), Name :: binary()) ->
    ok | {error, term()}.
remove_dns_txt_record(Client, ProviderId, Name) ->
    entity_logic:handle(#el_req{
        operation = delete,
        client = Client,
        gri = #gri{type = od_provider, id = ProviderId, aspect = {dns_txt_record, Name}}
    }).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve data related to domain config and subdomain delegation.
%% @end
%%--------------------------------------------------------------------
-spec get_domain_config(Client :: entity_logic:client(), ProviderId :: od_provider:id()) ->
    {ok, map()} | {error, term()}.
get_domain_config(Client, ProviderId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_provider, id = ProviderId, aspect = domain_config}}).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective users of given provider.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_users(Client :: entity_logic:client(), ProviderId :: od_provider:id()) ->
    {ok, [od_user:id()]} | {error, term()}.
get_eff_users(Client, ProviderId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_provider, id = ProviderId, aspect = eff_users}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective user among
%% effective users of given provider.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_user(Client :: entity_logic:client(), ProviderId :: od_provider:id(),
    UserId :: od_user:id()) -> {ok, #{}} | {error, term()}.
get_eff_user(Client, ProviderId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_user, id = UserId, aspect = instance, scope = protected},
        auth_hint = ?THROUGH_PROVIDER(ProviderId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective groups of given provider.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_groups(Client :: entity_logic:client(), ProviderId :: od_provider:id()) ->
    {ok, [od_group:id()]} | {error, term()}.
get_eff_groups(Client, ProviderId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_provider, id = ProviderId, aspect = eff_groups}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective group among
%% effective groups of given provider.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_group(Client :: entity_logic:client(), ProviderId :: od_provider:id(),
    GroupId :: od_group:id()) -> {ok, #{}} | {error, term()}.
get_eff_group(Client, ProviderId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_group, id = GroupId, aspect = instance, scope = protected},
        auth_hint = ?THROUGH_PROVIDER(ProviderId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of spaces of given provider.
%% @end
%%--------------------------------------------------------------------
-spec get_spaces(Client :: entity_logic:client(), ProviderId :: od_provider:id()) ->
    {ok, [od_space:id()]} | {error, term()}.
get_spaces(Client, ProviderId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_provider, id = ProviderId, aspect = spaces}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific space among spaces of given provider.
%% @end
%%--------------------------------------------------------------------
-spec get_space(Client :: entity_logic:client(), ProviderId :: od_provider:id(),
    SpaceId :: od_space:id()) -> {ok, #{}} | {error, term()}.
get_space(Client, ProviderId, SpaceId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_space, id = SpaceId, aspect = instance, scope = protected},
        auth_hint = ?THROUGH_PROVIDER(ProviderId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates support size for specified space of given provider. Has two variants:
%% 1) New support size is given explicitly
%% 2) New support size is provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec update_support_size(Client :: entity_logic:client(), ProviderId :: od_provider:id(),
    SpaceId :: od_space:id(), SupSizeOrData :: integer() | #{}) -> ok | {error, term()}.
update_support_size(Client, ProviderId, SpaceId, SupSize) when is_integer(SupSize) ->
    update_support_size(Client, ProviderId, SpaceId, #{
        <<"size">> => SupSize
    });
update_support_size(Client, ProviderId, SpaceId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        client = Client,
        gri = #gri{type = od_provider, id = ProviderId, aspect = {space, SpaceId}},
        data = Data
    }).


%%--------------------------------------------------------------------
%% @doc
%% Revokes support for specified space on behalf of given provider.
%% @end
%%--------------------------------------------------------------------
-spec revoke_support(Client :: entity_logic:client(), ProviderId :: od_provider:id(),
    SpaceId :: od_space:id()) -> ok | {error, term()}.
revoke_support(Client, ProviderId, SpaceId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        client = Client,
        gri = #gri{type = od_provider, id = ProviderId, aspect = {space, SpaceId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Performs port check operation by requesting all specified URLs and returning
%% whether the requests succeeded.
%% @end
%%--------------------------------------------------------------------
-spec check_my_ports(Client :: entity_logic:client(), Data :: #{}) ->
    ok | {error, term()}.
check_my_ports(Client, Data) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        client = Client,
        gri = #gri{type = od_provider, id = undefined, aspect = check_my_ports},
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Returns current time in milliseconds, can be treated as synchronized across
%% the whole onezone with the accuracy of about one second.
%% Used to by providers to synchronize clocks with onezone (and thus each other).
%% @end
%%--------------------------------------------------------------------
-spec get_current_time(Client :: entity_logic:client()) ->
    {ok, non_neg_integer()} | {error, term()}.
get_current_time(Client) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_provider, id = undefined, aspect = current_time}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Verifies provider identity based on provided identity macaroon and alleged
%% provider id.
%% @end
%%--------------------------------------------------------------------
-spec verify_provider_identity(Client :: entity_logic:client(), od_provider:id(),
    Macaroon :: token:id() | macaroon:macaroon()) -> ok | {error, term()}.
verify_provider_identity(Client, ProviderId, Macaroon) ->
    verify_provider_identity(Client, #{
        <<"providerId">> => ProviderId,
        <<"macaroon">> => Macaroon
    }).


%%--------------------------------------------------------------------
%% @doc
%% Verifies provider identity based on provided identity macaroon and alleged
%% provider id. The macaroon and ProviderId are provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec verify_provider_identity(Client :: entity_logic:client(), entity_logic:data()) ->
    ok | {error, term()}.
verify_provider_identity(Client, Data) ->
    ?CREATE_RETURN_OK(entity_logic:handle(#el_req{
        operation = create,
        client = Client,
        gri = #gri{type = od_provider, id = undefined, aspect = verify_provider_identity},
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Returns whether a provider exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(ProviderId :: od_provider:id()) -> boolean().
exists(ProviderId) ->
    {ok, Exists} = od_provider:exists(ProviderId),
    Exists.


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified user is an effective user in given provider.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_user(ProviderOrId :: od_provider:id() | #od_provider{},
    UserId :: od_user:id()) -> boolean().
has_eff_user(ProviderId, UserId) when is_binary(ProviderId) ->
    case od_provider:get(ProviderId) of
        {ok, #document{value = Provider}} ->
            has_eff_user(Provider, UserId);
        _ ->
            false
    end;
has_eff_user(#od_provider{eff_users = EffUsers}, UserId) ->
    maps:is_key(UserId, EffUsers).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified group is an effective group in given
%% provider.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_group(ProviderOrId :: od_provider:id() | #od_provider{},
    GroupId :: od_group:id()) -> boolean().
has_eff_group(ProviderId, GroupId) when is_binary(ProviderId) ->
    case od_provider:get(ProviderId) of
        {ok, #document{value = Provider}} ->
            has_eff_group(Provider, GroupId);
        _ ->
            false
    end;
has_eff_group(#od_provider{eff_groups = EffGroups}, GroupId) ->
    maps:is_key(GroupId, EffGroups).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified space is supported by given provider.
%% @end
%%--------------------------------------------------------------------
-spec supports_space(ProviderOrId :: od_provider:id() | #od_provider{},
    SpaceId :: od_space:id()) -> boolean().
supports_space(ProviderId, SpaceId) when is_binary(ProviderId) ->
    case od_provider:get(ProviderId) of
        {ok, #document{value = Provider}} ->
            supports_space(Provider, SpaceId);
        _ ->
            false
    end;
supports_space(#od_provider{spaces = Spaces}, SpaceId) ->
    maps:is_key(SpaceId, Spaces).


%%--------------------------------------------------------------------
%% @doc
%% Returns full provider URL.
%% @end
%%--------------------------------------------------------------------
-spec get_url(ProviderId :: od_provider:id()) -> {ok, ProviderURL :: binary()}.
get_url(ProviderId) ->
    {ok, #od_provider{domain = Domain}} = get(?ROOT, ProviderId),
    {ok, str_utils:format_bin("https://~s", [Domain])}.


%%--------------------------------------------------------------------
%% @doc Returns provider id of provider that has been chosen
%% as default for given user, or {error, no_provider} otherwise.
%% If the user has a default spaces and it is supported by some providers,
%% one of them will be chosen randomly.
%% Otherwise, if any of user spaces is supported by any provider,
%% one of them will be chosen randomly.
%% @end
%%--------------------------------------------------------------------
-spec choose_provider_for_user(Referer :: binary() | undefined) ->
    {ok, ProviderId :: od_provider:id()} | {error, no_provider}.
choose_provider_for_user(UserId) ->
    % Check if the user has a default space and if it is supported.
    {ok, #od_user{
        spaces = Spaces, default_space = DefaultSpace
    }} = user_logic:get(?ROOT, UserId),
    {ok, DSProviders} =
        case DefaultSpace of
            undefined ->
                {ok, []};
            _ ->
                space_logic:get_providers(?ROOT, DefaultSpace)
        end,
    case DSProviders of
        List when length(List) > 0 ->
            % Default space has got some providers, random one
            {ok, lists:nth(rand:uniform(length(DSProviders)), DSProviders)};
        _ ->
            % Default space does not have a provider, look in other spaces
            ProviderIds = lists:foldl(
                fun(Space, Acc) ->
                    {ok, Providers} = space_logic:get_providers(?ROOT, Space),
                    Providers ++ Acc
                end, [], Spaces),

            case ProviderIds of
                [] ->
                    % No provider for other spaces = nowhere to redirect
                    {error, no_provider};
                _ ->
                    % There are some providers for other spaces, random one
                    {ok, lists:nth(rand:uniform(length(ProviderIds)), ProviderIds)}
            end
    end.

