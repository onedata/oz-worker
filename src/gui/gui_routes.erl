%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module provides mapping of globalregistry paths to modules
%%% that will render the pages.
%%% @end
%%%-------------------------------------------------------------------
-module(gui_routes).
-author("Tomasz Lichon").

-include("gui/common.hrl").
-include_lib("n2o/include/wf.hrl").

%% API
-export([init/2, finish/2]).

%%%===================================================================
%%% API
%%%===================================================================

finish(State, Ctx) -> {ok, State, Ctx}.

init(State, Ctx) ->
    Path = wf:path(Ctx#context.req),
    RequestedPage = case Path of
                        <<"/ws", Rest/binary>> -> Rest;
                        Other -> Other
                    end,
    {ok, State, Ctx#context{path = Path, module = route(RequestedPage)}}.

route(<<"/">>) -> page_manage_account;
route(<<"/login">>) -> page_login;
route(<<"/logout">>) -> page_logout;
route(<<"/validate_login">>) -> page_validate_login;
route(<<"/manage_account">>) -> page_manage_account;
route(<<"/about">>) -> page_about;
route(<<?privacy_policy_url>>) -> page_privacy_policy;
route(<<?become_a_provider_url>>) -> page_become_a_provider;
route(<<"/error">>) -> page_error;
route(<<"/auth_endpoint">>) -> page_auth_endpoint;
route(_) -> page_404.