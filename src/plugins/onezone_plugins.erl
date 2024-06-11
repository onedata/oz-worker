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
%%%
%%% All custom plugins are expected to be found in the directory
%%% /etc/oz_worker/plugins, and must be erlang files with ".erl" extension.
%%% They will be loaded upon Onezone startup. When using a deployment with more
%%% than one node, the same plugins must be provisioned on all nodes.
%%%
%%% The /etc/oz_worker/plugins directory contains some predefined plugins to be
%%% used as reference, containing those specific for different projects within
%%% which Onedata is used.
%%%
%%% Plugins must conform to predefined API that is specified in erlang behaviour
%%% modules. Refer to the oz-worker source code for the behaviours and
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
%%%   harvesting_backend - must implement harvesting_backend_behaviour
%%%
%%%   handle_metadata_plugin - must implement handle_metadata_plugin_behaviour
%%%
%%% See the corresponding behaviours for more info.
%%%
%%% NOTE: some of the above behaviours are not only implemented by custom plugins, but
%%% also by internal modules that provide built-in functionalities.
%%%
%%% Some plugins support validation examples that will be evaluated upon Onezone startup
%%% and the results will be logged in Onezone logs. They serve as a form of unit tests.
%%% @end
%%%-------------------------------------------------------------------
-module(onezone_plugins).

-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").


-define(PLUGINS_DIR, oz_worker:get_env(plugins_directory)).
-define(INCLUDES_DIR, filename:join(code:lib_dir(?APP_NAME), "include")).
-define(COMPILE_OPTS, [return_errors, {i, ?INCLUDES_DIR}]).
-define(PLUGINS_KEY, onezone_plugins).

-export([init/0, get_plugins/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns all loaded plugins of specified type. 
%% @end
%%--------------------------------------------------------------------
-spec get_plugins(onezone_plugin_behaviour:type()) -> [module()].
get_plugins(Type) ->
    AllPlugins = node_cache:get(?PLUGINS_KEY),
    [P || P <- AllPlugins, P:type() =:= Type].


%%--------------------------------------------------------------------
%% @doc
%% Loads all plugin files that are found in the plugin directory and performs
%% validation, if possible.
%% @end
%%--------------------------------------------------------------------
-spec init() -> boolean().
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

    ValidationResults = lists:map(fun(Plugin) ->
        try
            {ok, Module} = compile:file(filename:join(PluginsDir, Plugin), ?COMPILE_OPTS),
            code:purge(Module),
            {module, Module} = code:load_file(Module),
            lists:member(Module:type(), allowed_plugin_types()) orelse error({unknown_plugin_type, Module:type()}),
            case validate_plugin(Module) of
                ok ->
                    ?info("  -> ~p: successfully loaded", [Module]),
                    {ok, Module};
                error ->
                    ?warning("  -> ~p: plugin was loaded, but failed validation", [Module]),
                    error
            end
        catch Class:Reason:Stacktrace ->
            ?error_exception("Cannot load ~s plugin", [Plugin], Class, Reason, Stacktrace),
            error
        end
    end, PluginFiles),

    node_cache:put(?PLUGINS_KEY, [Plugin || {ok, Plugin} <- ValidationResults]),
    lists:all(fun(Res) -> Res =/= error end, ValidationResults).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec allowed_plugin_types() -> [onezone_plugin_behaviour:type()].
allowed_plugin_types() ->
    [entitlement_parser, openid_plugin, attribute_mapper, harvesting_backend, handle_metadata_plugin].


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Validates user-defined auth plugin depending on its type.
%% @end
%%--------------------------------------------------------------------
-spec validate_plugin(module()) -> ok | error.
validate_plugin(Module) ->
    validate_plugin(Module, Module:type()).

-spec validate_plugin(module(), onezone_plugin_behaviour:type()) -> ok | error.
validate_plugin(_, openid_plugin) ->
    % openid_plugin does not undergo validation
    ok;
validate_plugin(_, harvesting_backend) ->
    % harvesting_backend does not undergo validation
    ok;
validate_plugin(Module, Type) ->
    try
        Examples = Module:validation_examples(),
        ?info("  -> ~p: found ~B validation examples for plugin, testing...", [
            Module, length(Examples)
        ]),
        ValidationFunction = case Type of
            entitlement_parser -> fun entitlement_parser_behaviour:validate_example/2;
            attribute_mapper -> fun attribute_mapper_behaviour:validate_example/2;
            handle_metadata_plugin -> fun handle_metadata_plugin_behaviour:validate_example/2
        end,
        [erlang:apply(ValidationFunction, [Module, E]) || E <- Examples],
        ?info("  -> ~p: all validation examples passed", [Module])
    catch Class:Reason:Stacktrace ->
        {Class, Reason} /= {throw, validation_failed} andalso ?error_exception(Class, Reason, Stacktrace),
        ?error("  -> ~p: failed to test validation examples (see the error above)", [Module]),
        error
    end.
