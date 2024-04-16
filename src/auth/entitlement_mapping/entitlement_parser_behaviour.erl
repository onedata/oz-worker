%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This behaviour specifies the API for an auth plugin that maps IdP
%%% entitlements into Onedata entitlements (group memberships).
%%% @end
%%%-------------------------------------------------------------------
-module(entitlement_parser_behaviour).

-include_lib("ctool/include/logging.hrl").


-export([validate_example/2]).


%%--------------------------------------------------------------------
%% @doc
%% Parses an entitlement coming from given IdP into internal Onedata format.
%% @end
%%--------------------------------------------------------------------
-callback parse(auth_config:idp(), entitlement_mapping:raw_entitlement(), auth_config:parser_config()) ->
    entitlement_mapping:idp_entitlement().


%%--------------------------------------------------------------------
%% @doc
%% Returns validation examples that will be tested when the plugin is loaded.
%% They serve as unit tests for the plugin.
%% @end
%%--------------------------------------------------------------------
-callback validation_examples() ->
    [{auth_config:idp(), entitlement_mapping:raw_entitlement(), auth_config:parser_config(),
    entitlement_mapping:idp_entitlement() | {error, malformed}}].


%%%===================================================================
%%% API
%%%===================================================================


-spec validate_example(module(), {auth_config:idp(),
    entitlement_mapping:raw_entitlement(), auth_config:parser_config(),
    entitlement_mapping:idp_entitlement() | {error, malformed}}) -> ok | no_return().
validate_example(Module, {IdP, Input, ParserConfig, ExpectedOutput}) ->
    ParsingResult = try
        Module:parse(IdP, Input, ParserConfig)
    catch
        Type:Reason:Stacktrace ->
            {error, malformed, Type, Reason, Stacktrace}
    end,
    case {ExpectedOutput, ParsingResult} of
        {Same, Same} ->
            ok;
        {{error, malformed}, {error, malformed, _, _, _}} ->
            ok;
        {_, {error, malformed, EType, EReason, EStacktrace}} ->
            ?error("Validation example crashed:~n"
            "IdP: ~p~n"
            "Input: ~p~n"
            "ParserConfig: ~p~n"
            "Expected: ~p~n"
            "Error: ~p~n"
            "Stacktrace: ~s~n", [
                IdP, Input, ParserConfig, ExpectedOutput, {EType, EReason},
                iolist_to_binary(lager:pr_stacktrace(EStacktrace))
            ]),
            throw(validation_failed);
        {_, Got} ->
            ?error("Validation example failed:~n"
            "IdP: ~p~n"
            "Input: ~p~n"
            "ParserConfig: ~p~n"
            "Expected: ~p~n"
            "Got: ~p", [IdP, Input, ParserConfig, ExpectedOutput, Got]),
            throw(validation_failed)
    end.
