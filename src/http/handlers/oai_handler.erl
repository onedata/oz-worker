%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
-module(oai_handler).
-author("Jakub Kudzia").

-include_lib("ctool/include/logging.hrl").
-include("http/handlers/oai.hrl").
-include("http/handlers/oai_errors.hrl").
-include("http/handlers/rest_handler.hrl").


%% API
-export([init/3, terminate/3, rest_init/2, allowed_methods/2,
    content_types_accepted/2, content_types_provided/2, resource_exists/2,
    accept_resource/2, provide_resource/2, generate_response/2, count_key_occurrences/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Upgrade the protocol to cowboy_rest.
%% @end
%%--------------------------------------------------------------------
init({_, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Initialize the state for this request.
%% @end
%%--------------------------------------------------------------------
rest_init(Req, Opts) ->
    {ok, Req, Opts}.

%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Return the list of allowed methods.
%% @end
%%--------------------------------------------------------------------
allowed_methods(Req, State) ->
    {?ALLOWED_METHODS, Req, State}.


%% TODO malformed_request ???
%% TODO valid_entity_length ???


%% @doc Cowboy callback function.
%% Return the list of content-types the resource provides.
%% @end
%%--------------------------------------------------------------------
content_types_provided(Req, State) ->
    {[{?RESPONSE_CONTENT_TYPE, provide_resource}], Req, State}.


%% TODO charsets_provided ????

%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Return the list of content-types the resource accepts.
%% @end
%%--------------------------------------------------------------------
content_types_accepted(Req, State) ->
    {[{?REQUEST_CONTENT_TYPE, accept_resource}], Req, State}.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Return whether the resource exists.
%% @end
%%--------------------------------------------------------------------
resource_exists(Req, State) ->
    %%    TODO
    {true, Req, State}.


%%--------------------------------------------------------------------
%% @doc
%% Cowboy handler callback, no cleanup needed
%%--------------------------------------------------------------------
-spec terminate(term(), term(), term()) -> ok.
terminate(_Reason, _Req, _State) ->
    %%    TODO needed ?
    ok.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Process the request body.
%% @end
%%--------------------------------------------------------------------
-spec provide_resource(Req :: cowboy_req:req(), State :: any()) ->
    {iodata(), cowboy_req:req(), any()}.
provide_resource(Req, State) ->
    {QS, Req1} = cowboy_req:qs_vals(Req),
    {ResponseBody, Req2} = handle_request(QS, Req1),
    {ResponseBody, Req2, State}.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Process the request body of application/x-www-form-urlencoded content type.
%% @end
%%--------------------------------------------------------------------
-spec accept_resource(Req :: cowboy_req:req(), State :: any()) ->
    boolean() | {{true, URL :: binary()} | boolean(), cowboy_req:req(), any()}.
accept_resource(Req, State) ->
    {ok, QS, Req1} = cowboy_req:body_qs(Req),
    {ResponseBody, Req2} = handle_request(QS, Req1),
    Req3 = cowboy_req:set_resp_body(ResponseBody, Req2),
    {true, Req3, State}. % TODO delete content-type x-www-form .. from request


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% WRITEME
%% @end
%%--------------------------------------------------------------------
-spec verb_to_module(binary()) -> oai_verb().
verb_to_module(<<"Identify">>) -> identify;
verb_to_module(<<"GetRecord">>) -> get_record;
verb_to_module(<<"ListIdentifiers">>) -> list_identifiers;
verb_to_module(<<"ListMedatadaFormats">>) -> list_medatada_formats;
verb_to_module(<<"ListRecords">>) -> list_records;
verb_to_module(<<"ListSets">>) -> list_sets;
verb_to_module(_) -> badVerb.


handle_request(QueryString, Req) ->
    ResponseDate = generate_response_date_element(),
    {Response, RequestElement} =
        case key_occurs_exactly_once(<<"verb">>, QueryString) of
        false -> {to_xml(?BAD_VERB), generate_request_element(Req)};
        true ->
            Verb = proplists:get_value(<<"verb">>, QueryString),
            case verb_to_module(Verb) of
                badVerb -> {to_xml(?BAD_VERB), generate_request_element(Req)};
                Module ->
%%                    ArgsList = proplists:delete(<<"verb">>, QueryString),
                    case validate_arguments(Module, QueryString) of
                        true -> handle_request_with_validated_args(Verb, QueryString, Req);
                        false -> {to_xml(?BAD_ARGUMENT), generate_request_element(Req)}
                    end

            end
    end,
    XML = insert_to_root_xml_element([ResponseDate, RequestElement, Response]),
    %% io:format("DEBUG::~n~p~nDEBUG~n", [XML]),
    Prolog = ["<?xml version=\"1.0\" encoding=\"utf-8\" ?>"],
    ResponseBody = xmerl:export_simple([XML], xmerl_xml, [{prolog, Prolog}]),
    %% io:format(lists:flatten(xmerl:export_simple([XML], xmerl_xml))),
    Req2 = cowboy_req:set_resp_header(<<"content-type">>,
    ?RESPONSE_CONTENT_TYPE, Req),
    {ResponseBody, Req2}.


handle_request_with_validated_args(Verb, ArgsList, Req) ->
    %todo handle request specific errors
    Response = case generate_response(Verb, ArgsList) of
        {error, Reason} -> to_xml(Reason);
        PositiveResponse -> PositiveResponse
    end,

    {Response, generate_request_element(ArgsList, Req)}.

generate_response(Verb, Args) ->

    Module = verb_to_module(Verb),
    try
        RequiredElements = lists:flatmap(fun(ElementName) ->
            case Module:get_element(ElementName) of
                {error, Reason} ->
                    io:format("DEBUG: ~p~n", [Reason]),
                    throw(Reason);
                Element ->
                    io:format("DEBUG2: ~p~n", [Element]),
                    ensure_list(to_xml(ElementName, Element))
            end
            % TODO handle error in get_element
        end, Module:required_response_elements()),

        io:format("DEBUG3: ~p~n", [RequiredElements]),


        OptionalElements = lists:flatmap(fun(ElementName) ->
            try to_xml(ElementName, Module:get_element(ElementName)) of % TODO handle error in get_element
                XML -> ensure_list(XML)
            catch _:_ -> [] %todo handle if it fails with undefined or other error
            end
        end, Module:optional_response_elements()),

        #xmlElement{
            name = binary_to_atom(Verb, latin1),
            content = RequiredElements ++ OptionalElements
        }
    catch
%%        _:function_clause -> handle errors
        _:Error -> to_xml(Error)
    end.

insert_to_root_xml_element(Content) when is_list(Content) ->
    ?ROOT_ELEMENT#xmlElement{content = Content};
insert_to_root_xml_element(Content) ->
    ?ROOT_ELEMENT#xmlElement{content = [Content]}.



to_xml(#oai_error{}=Value) ->
    to_xml([], Value).


to_xml(Name, #xmlElement{} = Value) ->
    #xmlElement{name = Name, content = [Value]};
to_xml(Name, #oai_record{header = Header, metadata = Metadata}) ->
    #xmlElement{
        name = Name,
        content = ensure_list(to_xml(header, Header)) ++
                  ensure_list(to_xml(metadata, Metadata))}; % todo about is optional
to_xml(Name, #oai_header{identifier = Identifier, datestamp = Datestamp, setSpec = SetSpec}) ->
    #xmlElement{
        name = Name,
        content = ensure_list(to_xml(identifier, Identifier)) ++
                  ensure_list(to_xml(datestamp, Datestamp)) ++
                  ensure_list(to_xml(setSpec, SetSpec))};
to_xml(Name, #oai_metadata{metadata_format=Format, value=Value}) ->
    MetadataPrefix = Format#oai_metadata_format.metadataPrefix,
    Mod = metadata_prefix_to_metadata_format(MetadataPrefix),
    #xmlElement{name=Name, content=[Mod:encode(Value)]};
to_xml(_Name, #oai_error{code=Code, description=Description}) ->
    #xmlElement{
        name=error,
        attributes = [#xmlAttribute{name=code, value=Code}],
        content = [Description]
    };
to_xml(Name, [Value]) ->
    [to_xml(Name, Value)];
to_xml(Name, [Value | Values]) ->
    [to_xml(Name, Value) | to_xml(Name, Values)];
to_xml(Name, Value) when is_binary(Value) ->
    #xmlElement{name = Name, content = [binary_to_list(Value)]};
to_xml(Name, Value) when is_number(Value) ->
    #xmlElement{name = Name, content = [str_utils:format("~B", [Value])]};
to_xml(Name, Value) ->
    #xmlElement{name = Name, content = [Value]}.

generate_response_date_element() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:universal_time(),
    #xmlElement{name = responseDate,
        content = [str_utils:format(
            "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
            [Year, Month, Day, Hour, Minute, Second])]}.


generate_request_element(Req) ->
    generate_request_element([], Req).

generate_request_element(ParsedArgs, Req) ->
    {Path, Req2} = cowboy_req:path(Req),
    {URL, _Req3} = cowboy_req:host_url(Req2),
    BaseURL = string:concat(binary_to_list(URL), binary_to_list(Path)),

    #xmlElement{
        name = request,
        attributes = generate_attributes(ParsedArgs),
        content = [BaseURL]
    }.

generate_attributes(ParsedArgs) ->

    lists:map(fun({Name, Value}) ->
        #xmlAttribute{
            name = binary_to_atom(Name, latin1),
            value = binary_to_list(Value)}
    end, ParsedArgs).

validate_arguments(Module, QueryString) ->
    ArgsList = proplists:delete(<<"verb">>, QueryString),
    all_keys_occurs_exactly_once(ArgsList) and
    not illegal_arguments_exist(Module, ArgsList) and
    ( not exclusive_argument_exist(Module, ArgsList) and
        all_required_arguments_are_present(Module, ArgsList) )
    or (exclusive_argument_exist(Module, ArgsList) and (length(ArgsList) == 1)).


error_to_xml(#oai_error{code=Code, description=Description}) ->
    #xmlElement{
        name=error,
        attributes = [#xmlAttribute{name=code, value=Code}],
        content = [Description]
    }.

metadata_prefix_to_metadata_format(oai_dc) -> dublin_core.

exclusive_argument_exist(Module, ArgsList) ->
    ExclusiveArgumentsSet = sets:from_list(Module:exclusive_arguments()),
    ExistingArgumentsSet = sets:from_list(proplists:get_keys(ArgsList)),
    not sets:is_disjoint(ExclusiveArgumentsSet, ExistingArgumentsSet).

all_required_arguments_are_present(Module, ArgsList) ->

    RequiredArgumentsSet = sets:from_list(Module:required_arguments()),
    ExistingArgumentsSet = sets:from_list(proplists:get_keys(ArgsList)),
    sets:is_subset(RequiredArgumentsSet, ExistingArgumentsSet).

illegal_arguments_exist(Module, ArgsList) ->
    KnownArgumentsSet = sets:from_list(Module:required_arguments() ++ Module:optional_arguments()),
    ExistingArgumentsSet = sets:from_list(proplists:get_keys(ArgsList)),
    not sets:is_subset(ExistingArgumentsSet, KnownArgumentsSet).

all_keys_occurs_exactly_once(Proplist) ->
    lists:foldl(fun({K, _V}, Acc) ->
        key_occurs_exactly_once(K, Proplist) and Acc
    end, true, Proplist).


%%key exists and is not duplicated
key_occurs_exactly_once(Key, Proplist) ->
    case count_key_occurrences(Key, Proplist) of
        1 -> true;
        _ -> false
    end.

count_key_occurrences(Key, Proplist) ->
    lists:foldl(fun({K, _V}, Sum) ->
        case K of
            Key -> 1 + Sum;
            _ -> Sum
        end
    end, 0, Proplist).


ensure_list(Arg) when is_list(Arg) -> Arg;
ensure_list(Arg) -> [Arg].

%% TODO
%% TODO * OAI-identifier
%% TODO * docs
%% TODO * specs
%% TODO * error handling
%% TODO * handle datestamps
%% TODO * handle all verbs
%% TODO * compression
%% TODO * identity encoding
%% TODO * maybe parsing arguments should be implemented in each module to return proper error
%% TODO * handle error in get_element
%% TODO * allowed charset
%% TODO * error message should include info what exactly failed
%% TODO * what if processing optional argument fails, should i send xml error or not include
%% TODO   argument in response



