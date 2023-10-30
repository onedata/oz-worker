%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module contains utility functions for dns.
%%% @end
%%%-------------------------------------------------------------------
-module(dns_utils).
-author("Wojciech Geisler").

-export([build_domain/2, build_fqdn_from_subdomain/1]).

-type domain() :: binary().
-type subdomain() :: binary().

-export_type([domain/0, subdomain/0]).


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
-spec build_domain(Subdomain :: domain(), Domain :: domain()) -> domain().
build_domain(<<>>, Domain) ->
    string:lowercase(Domain);
build_domain(Subdomain, Domain) ->
    string:lowercase(<<Subdomain/binary, ".", Domain/binary>>).


%%--------------------------------------------------------------------
%% @doc
%% Joins provided subdomain with onezone domain.
%% @end
%%--------------------------------------------------------------------
-spec build_fqdn_from_subdomain(Subdomain :: domain()) -> domain().
build_fqdn_from_subdomain(Subdomain) ->
    build_domain(Subdomain, oz_worker:get_domain()).
