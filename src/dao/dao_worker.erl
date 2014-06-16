%% ===================================================================
%% @author Rafal Slota
%% @copyright (C): 2013 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module implements {@link worker_plugin_behaviour} callbacks and contains utility API methods. <br/>
%% DAO API functions are implemented in DAO sub-modules like: {@link dao_cluster}, {@link dao_vfs}. <br/>
%% All DAO API functions Should not be used directly, use {@link dao_worker:handle/2} instead.
%% Module :: atom() is module suffix (prefix is 'dao_'), MethodName :: atom() is the method name
%% and ListOfArgs :: [term()] is list of argument for the method. <br/>
%% If you want to call utility methods from this module - use Module = utils
%% See {@link dao_worker:handle/2} for more details.
%% @end
%% ===================================================================
-module(dao_worker).
-behaviour(gen_server).
-author("Rafal Slota").

-include_lib("dao/include/common.hrl").
-include_lib("dao/include/couch_db.hrl").
-include("dao/dao_driver.hrl").
-include("dao/dao_types.hrl").
-include("registered_names.hrl").

-import(dao_helper, [name/1]).

-ifdef(TEST).
-compile([export_all]).
-endif.

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-record(state, {}).

%% API
-export([start_link/0]).
-export([load_view_def/2]).


%%%===================================================================
%%% Start gen_server api
%%%===================================================================

%% start_link/0
%% ====================================================================
%% @doc Starts the server
%% ====================================================================
-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
%% ====================================================================
start_link() ->
	gen_server:start_link({local, ?Dao}, ?MODULE, [], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% init/1
%% ====================================================================
%% @doc Initializes the server
%% ====================================================================
-spec(init(Args :: term()) ->
	{ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
%% ====================================================================
init({Args, {init_status, undefined}}) ->
	ets:new(db_host_store, [named_table, public, bag, {read_concurrency, true}]),
	init({Args, {init_status, table_initialized}});
init({_Args, {init_status, table_initialized}}) -> %% Final stage of initialization. ETS table was initialized
	case application:get_env(?APP_Name, db_nodes) of
		{ok, Nodes} when is_list(Nodes) ->
			[dao_hosts:insert(Node) || Node <- Nodes, is_atom(Node)],
			catch setup_views(?DATABASE_DESIGN_STRUCTURE);
		_ ->
			lager:warning("There are no DB hosts given in application env variable.")
	end,
	{ok,#state{}};
init({Args, {init_status, _TableInfo}}) ->
	init({Args, {init_status, table_initialized}});
init(Args) ->
	init({Args, {init_status, ets:info(db_host_store)}}).

%% handle_call/3
%% ====================================================================
%% @doc Handling call messages
%% ====================================================================
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
	State :: #state{}) ->
	{reply, Reply :: term(), NewState :: #state{}} |
	{reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
	{stop, Reason :: term(), NewState :: #state{}}).
%% ====================================================================
handle_call({ProtocolVersion,Target, Method, Args},_From,State) when is_atom(Target), is_atom(Method), is_list(Args) ->
	put(protocol_version, ProtocolVersion), %% Some sub-modules may need it to communicate with DAO' gen_server
	Module =
		case atom_to_list(Target) of
			"utils" -> dao;
			[$d, $a, $o, $_ | T] -> list_to_atom("dao_" ++ T);
			T -> list_to_atom("dao_" ++ T)
		end,
	try apply(Module, Method, Args) of
		{error, Err} ->
			lager:error("Handling ~p:~p with args ~p returned error: ~p", [Module, Method, Args, Err]),
			{reply, {error, Err}, State};
		{ok, Response} -> {reply, {ok, Response}, State};
		ok -> {reply, ok, State};
		Other ->
			lager:error("Handling ~p:~p with args ~p returned unknown response: ~p", [Module, Method, Args, Other]),
			{reply, {error, Other}, State}
	catch
		error:{badmatch, {error, Err}} -> {reply, {error, Err}, State};
		Type:Error ->
            lager:error("Handling ~p:~p with args ~p interrupted by exception: ~p:~p ~n ~p", [Module, Method, Args, Type, Error, erlang:get_stacktrace()]),
			{reply, {error, Error}, State}
	end;
handle_call({ProtocolVersion, Method, Args},_From,State) when is_atom(Method), is_list(Args) ->
	{reply,gen_server:call(?Dao,{ProtocolVersion, cluster, Method, Args}),State};
handle_call(_Request,_From,State) ->
	lager:error("Unknown call request ~p ", [_Request]),
	{reply,{error, wrong_args},State}.

%% handle_cast/2
%% ====================================================================
%% @doc Handling cast messages
%% ====================================================================
-spec(handle_cast(Request :: term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).
%% ====================================================================
handle_cast(_Request, State) ->
	lager:error("Unknown cast request ~p ", [_Request]),
	{noreply, State}.

%% handle_info/2
%% ====================================================================
%% @doc Handling all non call/cast messages
%% ====================================================================
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).
%% ====================================================================
handle_info(_Info, State) ->
	lager:error("Unknown info request ~p ", [_Info]),
	{noreply, State}.

%% terminate/2
%% ====================================================================
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%% ====================================================================
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
	State :: #state{}) -> term()).
%% ====================================================================
terminate(_Reason, _State) ->
	ok.


%% code_change/3
%% ====================================================================
%% @doc Convert process state when code is changed
%% ====================================================================
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
	Extra :: term()) ->
	{ok, NewState :: #state{}} | {error, Reason :: term()}).
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================

%% setup_views/1
%% ====================================================================
%% @doc Creates or updates design documents
%% @end
-spec setup_views(DesignStruct :: list()) -> ok.
%% ====================================================================
setup_views(DesignStruct) ->
    DesignFun = fun(#design_info{name = Name, views = ViewList}, DbName) ->  %% Foreach design document
            LastCTX = %% Calculate MD5 sum of current views (read from files)
                lists:foldl(fun(#view_info{name = ViewName}, CTX) ->
                            crypto:hash_update(CTX, load_view_def(ViewName, map) ++ load_view_def(ViewName, reduce))
                        end, crypto:hash_init(md5), ViewList),

            LocalVersion = dao_helper:name(integer_to_list(binary:decode_unsigned(crypto:hash_final(LastCTX)), 16)),
            NewViewList =
                case dao_helper:open_design_doc(DbName, Name) of
                    {ok, #doc{body = Body}} -> %% Design document exists, so lets calculate MD5 sum of its views
                        ViewsField = dao_json:get_field(Body, "views"),
                        DbViews = [ dao_json:get_field(ViewsField, ViewName) || #view_info{name = ViewName} <- ViewList ],
                        EmptyString = fun(Str) when is_binary(Str) -> binary_to_list(Str); %% Helper function converting non-string value to empty string
                                         (_) -> "" end,
                        VStrings = [ EmptyString(dao_json:get_field(V, "map")) ++ EmptyString(dao_json:get_field(V, "reduce")) || {L}=V <- DbViews, is_list(L)],
                        LastCTX1 = lists:foldl(fun(VStr, CTX) -> crypto:hash_update(CTX, VStr) end, crypto:hash_init(md5), VStrings),
                        DbVersion = dao_helper:name(integer_to_list(binary:decode_unsigned(crypto:hash_final(LastCTX1)), 16)),
                        case DbVersion of %% Compare DbVersion with LocalVersion
                            LocalVersion ->
                                lager:info("DB version of design ~p is ~p and matches local version. Design is up to date", [Name, LocalVersion]),
                                [];
                            _Other ->
                                lager:info("DB version of design ~p is ~p and does not match ~p. Rebuilding design document", [Name, _Other, LocalVersion]),
                                ViewList
                        end;
                    _ ->
                        lager:info("Design document ~p in DB ~p not exists. Creating...", [Name, DbName]),
                        ViewList
                end,

            lists:map(fun(#view_info{name = ViewName}) -> %% Foreach view
                case dao_helper:create_view(DbName, Name, ViewName, load_view_def(ViewName, map), load_view_def(ViewName, reduce), LocalVersion) of
                    ok ->
                        lager:info("View ~p in design ~p, DB ~p has been created.", [ViewName, Name, DbName]);
                    _Err ->
                        lager:error("View ~p in design ~p, DB ~p creation failed. Error: ~p", [ViewName, Name, DbName, _Err])
                end
            end, NewViewList),
            DbName
        end,

    DbFun = fun(#db_info{name = Name, designs = Designs}) -> %% Foreach database
            dao_helper:create_db(Name, []),
            lists:foldl(DesignFun, Name, Designs)
        end,

    lists:map(DbFun, DesignStruct),
    ok.

%% load_view_def/2
%% ====================================================================
%% @doc Loads view definition from file.
%% @end
-spec load_view_def(Name :: string(), Type :: map | reduce) -> string().
%% ====================================================================
load_view_def(Name, Type) ->
    case file:read_file(?VIEW_DEF_LOCATION ++ Name ++ (case Type of map -> ?MAP_DEF_SUFFIX; reduce -> ?REDUCE_DEF_SUFFIX end)) of
        {ok, Data} -> binary_to_list(Data);
        _ -> ""
    end.
