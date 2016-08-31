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
-include("http/handlers/rest_handler.hrl").


%% API
-export([init/3, terminate/3, rest_init/2, allowed_methods/2,
    content_types_accepted/2, content_types_provided/2, resource_exists/2,
    accept_resource/2, provide_resource/2, generate_response/2]).

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
verb_to_module(<<"ListSets">>) -> list_sets.

handle_request(QueryString, Req) ->
    case proplists:get_value(<<"verb">>, QueryString) of
        undefined -> error; % todo handle bad verb error;
        Verb ->
            Module = verb_to_module(Verb),
            %%            ParsedArgs = Module:parse_arguments(Args),
            ParsedArgs = parse_args(Module, QueryString),
            Response = generate_response(Verb, ParsedArgs),
            ResponseDate = generate_response_date_element(), %todo handle errors
            RequestElement = generate_request_element(ParsedArgs, Req),
            XML = ?ROOT_ELEMENT#xmlElement{
                content = [ResponseDate, RequestElement, Response]},
            %%            io:format("DEBUG::~n~p~nDEBUG~n", [XML]),
            ResponseBody = xmerl:export_simple([XML], xmerl_xml),
            %%            io:format(lists:flatten(xmerl:export_simple([XML], xmerl_xml))),
            Req2 = cowboy_req:set_resp_header(<<"content-type">>,
                ?RESPONSE_CONTENT_TYPE, Req),
            {ResponseBody, Req2}
    end.


generate_response(Verb, Args) ->

    Module = verb_to_module(Verb),
    RequiredElements = lists:flatmap(fun(ElementName) ->
        XML =  to_xml(ElementName,
            Module:get_element(ElementName)), % TODO handle error in get_element
        io:format("~nNAME: ~p~n~p~n", [ElementName, XML]), XML
    end, Module:required_response_elements()),

    OptionalElements = lists:flatmap(fun(ElementName) ->
        try to_xml(ElementName, Module:get_element(
            ElementName)) of % TODO handle error in get_element
            XML -> XML
        catch _:_ -> []
        end
    end, Module:optional_response_elements()),
    #xmlElement{
        name = binary_to_atom(Verb, latin1),
        content = RequiredElements ++ OptionalElements
    }.


to_xml(Name, [#xmlElement{} = Value]) ->
    [#xmlElement{name = Name, content = [Value]}];
to_xml(Name, [#oai_record{header = Header, metadata = Metadata, about = About}]) ->
    [#xmlElement{name = Name,
        content = [
            to_xml(header, Header),
            to_xml(metadata, Metadata),
            to_xml(about, About)]}];
to_xml(Name, [#oai_header{identifier = Identifier, datestamp = Datestamp, setSpec = SetSpec}]) ->
    [#xmlElement{name = Name,
        content = [
            to_xml(identifier, Identifier),
            to_xml(datestamp, Datestamp),
            to_xml(setSpec, SetSpec)]}];
to_xml(Name, [Value, Values]) ->
    to_xml(Name, Value) ++  to_xml(Name, Values);
to_xml(Name, Value) when is_binary(Value) ->
    [#xmlElement{name = Name, content = [binary_to_list(Value)]}];
to_xml(Name, Value) when is_number(Value) ->
    [#xmlElement{name = Name, content = [str_utils:format("~B~", [Value])]}];
to_xml(Name, Value) ->
    [#xmlElement{name = Name, content = [Value]}].

generate_response_date_element() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:universal_time(),
    #xmlElement{name = responseDate,
        content = [str_utils:format(
            "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
            [Year, Month, Day, Hour, Minute, Second])]}. % TODO padding with 0


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

parse_args(Module, QueryString) ->
    %% TODO handle badargs, args repetition, illegal args etc.
    QueryString.
%%    RequiredArguments = lists:flatmap(fun(A) ->
%%        case proplists:get_value(QueryString, A) of
%%            undefined -> error;
%%            Value -> Value
%%        end
%%    end, Module:required_arguments()),
%%
%%    OptionalAttributes = lists:flatmap(fun(A) ->
%%    try generate_xml(A, Module:get_attribute(A)) of
%%        XML -> XML
%%        catch _:_ -> []
%%    end
%%    end, Module:optional_response_attributes()),
%%    RequiredArguments ++ OptionalAttributes.





%% TODO

%% TODO * OAI-identifier
%% TODO * response should contain: response data, request container
%% TODO * docs
%% TODO * specs
%% TODO * error handling
%% TODO * generating xml in proper format DC, ...
%% TODO     * it should be a separate module
%% TODO * add encoding to head of xml
%% TODO * handle datestamps
%% TODO * handle all verbs
%% TODO * compression
%% TODO * identity encoding
%% TODO * maybe parsing arguments should be implemented in each module to return proper error
% TODO handle error in get_element