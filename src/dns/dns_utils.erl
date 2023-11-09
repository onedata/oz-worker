%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2023 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module contains utility functions for dns.
%%% @end
%%%-------------------------------------------------------------------
-module(dns_utils).
-author("Bartosz Walkowicz").

-export([
    build_domain/2,
    build_fqdn_from_subdomain/1,

    is_equal_or_subdomain/2
]).

% A string, up to 63 characters long, build of alphanumerics and hyphens.
-type domain_label() :: binary().
% A domain name consists of one or more labels, that are delimited by dots,
% such as 'example.com'.
-type domain_name() :: binary().

-export_type([domain_label/0, domain_name/0]).


%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Joins subdomain with domain to build fully qualified domain.
%% Makes the string lowercase to provide some normalization to
%% names given in custom (app.config) entries.
%% Note that the Onezone domain is expected to be already in lowercase
%% and is not always passed through this function.
%% @end
%%--------------------------------------------------------------------
-spec build_domain(Subdomain :: domain_name(), Domain :: domain_name()) -> domain_name().
build_domain(<<>>, Domain) ->
    string:lowercase(Domain);
build_domain(Subdomain, Domain) ->
    string:lowercase(<<Subdomain/binary, ".", Domain/binary>>).


%%--------------------------------------------------------------------
%% @doc
%% Joins provided subdomain with onezone domain.
%% @end
%%--------------------------------------------------------------------
-spec build_fqdn_from_subdomain(Subdomain :: domain_name()) -> domain_name().
build_fqdn_from_subdomain(Subdomain) ->
    build_domain(Subdomain, oz_worker:get_domain()).


-spec is_equal_or_subdomain(domain_label(), domain_label()) -> boolean().
is_equal_or_subdomain(Domain, ReferenceDomain) ->
    DomainSize = byte_size(Domain),
    ReferenceDomainSize = byte_size(ReferenceDomain),

    if
        DomainSize > ReferenceDomainSize ->
            PrefixSize = DomainSize - ReferenceDomainSize - 1,

            case Domain of
                <<_Prefix:PrefixSize/binary, ".", ReferenceDomain/binary>> -> true;
                _ -> false
            end;

        DomainSize == ReferenceDomainSize ->
            Domain == ReferenceDomain;

        true ->
            false
    end.
