%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Plugins are user-defined erlang modules that can be injected into the
%%% Onezone service and used to customize it.
%%% All plugins are expected to be found in the directory
%%% /etc/oz_worker/plugins, and must be erlang files with ".erl" extension.
%%% They will be loaded upon Onezone startup. When using a deployment with more
%%% than one node, the same plugins must be provisioned on all nodes.
%%%
%%% Plugins must conform to predefined API that is specified in erlang behaviour
%%% modules. Please refer to the oz-worker source code for the behaviours and
%%% implementation guide.
%%%
%%% Each plugin must implement the 'onezone_plugin_behaviour', which has one
%%% callback - type/0, that returns the type of the plugin:
%%%
%%%   attribute_mapper - must implement attribute_mapper_behaviour
%%%
%%%   entitlement_parser - must implement entitlement_parser_behaviour
%%%
%%%   openid_plugin - must implement openid_plugin_behaviour
%%%
%%% See the corresponding behaviours for more info.
%%% entitlement_parser and attribute_mapper support validation examples that
%%% will be evaluated upon startup and the results will be logged in Onezone logs.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(onezone_plugins).

-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").

-define(PLUGINS_DIR, begin {ok, __Path} = oz_worker:get_env(plugins_directory), __Path end).
-define(INCLUDES_DIR, filename:join(code:lib_dir(?APP_NAME), "include")).
-define(COMPILE_OPTS, [return_errors, {i, ?INCLUDES_DIR}]).
-define(PLUGINS_KEY, onezone_plugins).

-export([init/0, get_all_plugins/0, get_plugins/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns all loaded plugins. 
%% @end
%%--------------------------------------------------------------------
-spec get_all_plugins() -> {ok, [module()]}.
get_all_plugins() ->
    simple_cache:get(?PLUGINS_KEY).


%%--------------------------------------------------------------------
%% @doc
%% Returns all loaded plugins of specified type. 
%% @end
%%--------------------------------------------------------------------
-spec get_plugins(Type :: atom()) -> [module()].
get_plugins(Type) ->
    {ok, AllPlugins} = get_all_plugins(),
    [P || P <- AllPlugins, P:type() =:= Type].


%%--------------------------------------------------------------------
%% @doc
%% Loads all plugin files that are found in the plugin directory and performs
%% validation, if possible.
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok.
init() ->
    PluginsDir = ?PLUGINS_DIR,
    PluginFiles = case file:list_dir(PluginsDir) of
        {error, Error} ->
            ?warning("Cannot read plugins directory, no plugins will be loaded: ~p", [
                {error, Error}
            ]),
            [];
        {ok, Files} ->
            ErlFiles = [F || F <- Files, filename:extension(F) == ".erl"],
            case length(ErlFiles) of
                0 -> ?info("No plugins found in ~s", [PluginsDir]);
                N -> ?info("Found ~B plugins in ~s", [N, PluginsDir])
            end,
            ErlFiles
    end,
    Plugins = lists:filtermap(fun(Plugin) ->
        try
            {ok, Module} = compile:file(filename:join(PluginsDir, Plugin), ?COMPILE_OPTS),
            code:purge(Module),
            {module, Module} = code:load_file(Module),
            validate_plugin(Module),
            ?info("  -> ~p: successfully loaded", [Module]),
            {true, Module}  
        catch Type:Reason ->
            ?error_stacktrace("Cannot load ~s plugin due to ~p:~p", [Plugin, Type, Reason]),
            false
        end
    end, PluginFiles),
    simple_cache:put(?PLUGINS_KEY, Plugins).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Validates user-defined auth plugin depending on its type.
%% @end
%%--------------------------------------------------------------------
-spec validate_plugin(module()) -> ok.
validate_plugin(Module) ->
    validate_plugin(Module, Module:type()).

-spec validate_plugin(module(), onezone_plugin_behaviour:type()) -> ok.
validate_plugin(Module, entitlement_parser) ->
    try Module:validation_examples() of
        Examples ->
            ?info("  -> ~p: found ~B validation examples for plugin, testing...", [
                Module, length(Examples)
            ]),
            [validate_entitlement_parsing_example(Module, E) || E <- Examples],
            ?info("  -> ~p: all validation examples passed", [Module])
    catch _:_ ->
        ?info("  -> ~p: no validation examples found for ~p", [Module])
    end;
validate_plugin(Module, attribute_mapper) ->
    try Module:validation_examples() of
        Examples ->
            ?info("  -> ~p: found ~B validation examples for plugin, testing...", [
                Module, length(Examples)
            ]),
            [validate_attribute_mapping_example(Module, E) || E <- Examples],
            ?info("  -> ~p: all validation examples passed", [Module])
    catch _:_ ->
        ?info("  -> ~p: no validation examples found for ~p", [Module])
    end;
validate_plugin(_, openid_plugin) ->
    % openid_plugin does not undergo validation
    ok;
validate_plugin(_, harvester_plugin) ->
    % harvester_plugin does not undergo validation
    ok;
validate_plugin(_, Type) ->
    throw({bad_plugin_type, Type}).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Validates a single entitlement parsing example.
%% @end
%%--------------------------------------------------------------------
-spec validate_entitlement_parsing_example(module(), {auth_config:idp(),
    entitlement_mapping:raw_entitlement(), auth_config:parser_config(),
    entitlement_mapping:idp_entitlement() | {error, malformed}}) -> ok.
validate_entitlement_parsing_example(Module, {IdP, Input, ParserConfig, ExpectedOutput}) ->
    ParsingResult = try
        Module:parse(IdP, Input, ParserConfig)
    catch
        Type:Reason ->
            {error, malformed, Type, Reason, erlang:get_stacktrace()}
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
            throw({validation_failed, IdP});
        {_, Got} ->
            ?error("Validation example failed:~n"
            "IdP: ~p~n"
            "Input: ~p~n"
            "ParserConfig: ~p~n"
            "Expected: ~p~n"
            "Got: ~p", [IdP, Input, ParserConfig, ExpectedOutput, Got]),
            throw({validation_failed, IdP})
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Validates a single attribute mapping example.
%% @end
%%--------------------------------------------------------------------
-spec validate_attribute_mapping_example(module(), {auth_config:idp(),
    attribute_mapping:onedata_attribute(), attribute_mapping:idp_attributes(),
    {ok, term()} | {error, not_found} | {error, attribute_mapping_error}}) -> ok.
validate_attribute_mapping_example(Module, {IdP, Attribute, IdPAttributes, ExpectedOutput}) ->
    ParsingResult = try
        Module:map_attribute(IdP, Attribute, IdPAttributes)
    catch
        Type:Reason ->
            {error, attribute_mapping_error, Type, Reason, erlang:get_stacktrace()}
    end,
    case {ExpectedOutput, ParsingResult} of
        {Same, Same} ->
            ok;
        {{error, attribute_mapping_error}, {error, attribute_mapping_error, _, _, _}} ->
            ok;
        {_, {error, attribute_mapping_error, EType, EReason, EStacktrace}} ->
            ?error("Validation example crashed:~n"
            "IdP: ~p~n"
            "Attribute: ~p~n"
            "IdPAttributes: ~p~n"
            "Expected: ~p~n"
            "Error: ~p~n"
            "Stacktrace: ~s~n", [
                IdP, Attribute, IdPAttributes, ExpectedOutput, {EType, EReason},
                iolist_to_binary(lager:pr_stacktrace(EStacktrace))
            ]),
            throw({validation_failed, IdP});
        {_, Got} ->
            ?error("Validation example failed:~n"
            "IdP: ~p~n"
            "Attribute: ~p~n"
            "IdPAttributes: ~p~n"
            "Expected: ~p~n"
            "Got: ~p", [IdP, Attribute, IdPAttributes, ExpectedOutput, Got]),
            throw({validation_failed, IdP})
    end.