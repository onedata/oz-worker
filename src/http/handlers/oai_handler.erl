%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% This module is responsible for handling OAI-PMH requests.
%%% It implements Cowboy REST handler behaviour.
%%% @end
%%%-------------------------------------------------------------------
-module(oai_handler).
-author("Jakub Kudzia").

-behaviour(cowboy_rest).

-include("http/handlers/oai.hrl").
-include_lib("ctool/include/logging.hrl").

%%% API
-export([
    init/2, allowed_methods/2, content_types_accepted/2,
    content_types_provided/2, accept_resource/2, provide_resource/2,
    generate_response_date_element/0
]).


%%%===================================================================
%%% API
%%%===================================================================

%%%--------------------------------------------------------------------
%%% @doc Cowboy callback function.
%%% Upgrade the protocol to cowboy_rest.
%%% @end
%%%--------------------------------------------------------------------
-spec init(Req :: cowboy_req:req(), Opts :: any()) ->
    {cowboy_rest, cowboy_req:req(), any()}.
init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.


%%%--------------------------------------------------------------------
%%% @doc Cowboy callback function.
%%% Return the list of allowed methods.
%%% @end
%%%--------------------------------------------------------------------
-spec allowed_methods(Req :: cowboy_req:req(), State :: any()) ->
    {[binary()], cowboy_req:req(), any()}.
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.


%%%--------------------------------------------------------------------
%%% @doc Cowboy callback function.
%%% Return the list of content-types the resource provides.
%%% @end
%%%--------------------------------------------------------------------
-spec content_types_provided(cowboy_req:req(), any()) ->
    {Value, cowboy_req:req(), any()} when
    Value :: [{binary() | {Type, SubType, Params}, ProvideResource}],
    Type :: binary(),
    SubType :: binary(),
    Params :: '*' | [{binary(), binary()}],
    ProvideResource :: atom().
content_types_provided(Req, State) ->
    {[{<<"text/xml">>, provide_resource}], Req, State}.


%%%--------------------------------------------------------------------
%%% @doc Cowboy callback function.
%%% Return the list of content-types the resource accepts.
%%% @end
%%%--------------------------------------------------------------------
-spec content_types_accepted(cowboy_req:req(), any()) ->
    {Value, cowboy_req:req(), any()} when
    Value :: [{binary() | {Type, SubType, Params}, AcceptResource}],
    Type :: binary(),
    SubType :: binary(),
    Params :: '*' | [{binary(), binary()}],
    AcceptResource :: atom().
content_types_accepted(Req, State) ->
    {[{<<"application/x-www-form-urlencoded">>, accept_resource}], Req, State}.


%%%--------------------------------------------------------------------
%%% @doc Cowboy callback function.
%%% Return the response body.
%%% @end
%%%--------------------------------------------------------------------
-spec provide_resource(Req :: cowboy_req:req(), State :: any()) ->
    {iodata() | stop, cowboy_req:req(), any()}.
provide_resource(Req, State) ->
    QueryParams = cowboy_req:parse_qs(Req),
    try
        {ResponseBody, Req2} = handle_request(QueryParams, Req),
        {ResponseBody, Req2, State}
    catch
        throw:{stop, ReqE} ->
            {stop, ReqE, State}
    end.

%%%--------------------------------------------------------------------
%%% @doc Cowboy callback function.
%%% Process the request body.
%%% @end
%%%--------------------------------------------------------------------
-spec accept_resource(Req :: cowboy_req:req(), State :: any()) ->
    {{true | stop, URL :: binary()} | boolean(), cowboy_req:req(), any()}.
accept_resource(Req, State) ->
    {ok, QS, Req1} = cowboy_req:read_urlencoded_body(Req),
    try
        {ResponseBody, Req2} = handle_request(QS, Req1),
        Req3 = cowboy_req:set_resp_body(ResponseBody, Req2),
        {true, Req3, State}
    catch
        throw:{stop, ReqE} ->
            {stop, ReqE, State}
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%--------------------------------------------------------------------
%%% @private
%%% @doc
%%% This function is responsible for handling OAI-PMH requests.
%%% If known exception is caught it's redirected to oai_errors module.
%%% In case uf unknown error, it's logged and Internal Server Error is
%%% returned.
%%% @end
%%%--------------------------------------------------------------------
-spec handle_request(QueryParams :: [proplists:property()], Req :: cowboy_req:req()) -> tuple().
handle_request(QueryParams, Req) ->
    Response = try
        {Verb, ParsedArgs} = oai_parser:process_and_validate_args(QueryParams),
        generate_response(Verb, ParsedArgs)
    catch
        throw:Error ->
            oai_errors:handle(Error);
        ErrorType:Error ->
            ?error_stacktrace("Unhandled exception in OAI-PMH request ~p:~p", [ErrorType, Error]),
            ReqE = cowboy_req:reply(500, Req),
            throw({stop, ReqE})
    end,

    RequestElement = case Response of
        #oai_error{code = badVerb} -> generate_request_element(Req);
        #oai_error{code = badArgument} -> generate_request_element(Req);
        _ -> generate_request_element(QueryParams, Req)
    end,

    ResponseDate = oai_handler:generate_response_date_element(),

    ResponseXML = oai_utils:to_xml(Response),
    RequestElementXML = oai_utils:to_xml(RequestElement),
    ResponseDateXML = oai_utils:to_xml(ResponseDate),

    XML = insert_to_root_xml_element([ResponseDateXML, RequestElementXML, ResponseXML]),
    Prolog = ["<?xml version=\"1.0\" encoding=\"utf-8\" ?>"],
    ResponseBody = xmerl:export_simple([XML], xmerl_xml, [{prolog, Prolog}]),
    Req2 = cowboy_req:set_resp_header(<<"content-type">>, ?RESPONSE_CONTENT_TYPE, Req),
    {ResponseBody, Req2}.

%%%--------------------------------------------------------------------
%%% @private
%%% @doc
%%% This function is responsible for generating response to given
%%% OAI-PMH Verb (request).
%%% @end
%%%--------------------------------------------------------------------
-spec generate_response(Verb :: binary(), Args :: [proplists:property()]) ->
    {binary(), [{binary(), oai_response()}]}.
generate_response(Verb, Args) ->
    Module = oai_utils:verb_to_module(Verb),
    RequiredElements = generate_required_response_elements(Module, Args),
    OptionalElements = generate_optional_response_elements(Module, Args),
    {Verb, RequiredElements ++ OptionalElements}.

%%%--------------------------------------------------------------------
%%% @private
%%% @doc
%%% This function is responsible for generating required elements
%%% of response to given OAI-PMH request handled by Module.
%%% @end
%%%--------------------------------------------------------------------
-spec generate_required_response_elements(Module :: oai_verb_module(),
    Args :: [proplists:property()]) -> [{binary(), oai_response()}].
generate_required_response_elements(Module, Args) ->
    lists:flatmap(fun(ElementName) ->
        case Module:get_response(ElementName, Args) of
            Elements when is_list(Elements) ->
                [{ElementName, Element} || Element <- Elements];
            Element -> oai_utils:ensure_list({ElementName, Element})
        end
    end, Module:required_response_elements()).


%%%--------------------------------------------------------------------
%%% @private
%%% @doc
%%% This function is responsible for generating optional elements
%%% of response to given OAI-PMH request handled by Module.
%%% @end
%%%--------------------------------------------------------------------
-spec generate_optional_response_elements(Module :: oai_verb_module(),
    Args :: [proplists:property()]) -> [{binary(), oai_response()}].
generate_optional_response_elements(Module, Args) ->
    lists:flatmap(fun(ElementName) ->
        try Module:get_response(ElementName, Args) of
            <<"">> -> [];
            [] -> [];
            Elements when is_list(Elements) ->
                [{ElementName, Element} || Element <- Elements];
            Element -> oai_utils:ensure_list({ElementName, Element})
        catch _:_ -> []
        end
    end, Module:optional_response_elements()).

%%%--------------------------------------------------------------------
%%% @private
%%% @doc
%%% Inserts Content as content of root XML.
%%% @end
%%%--------------------------------------------------------------------
-spec insert_to_root_xml_element([#xmlElement{}]) -> #xmlElement{}.
insert_to_root_xml_element(Content) when is_list(Content) ->
    ?ROOT_ELEMENT#xmlElement{content = Content}.

%%%--------------------------------------------------------------------
%%% @private
%%% @doc
%%% Generate element of response element which contains response timestamp.
%%% @end
%%%--------------------------------------------------------------------
-spec generate_response_date_element() -> {atom(), binary()}.
generate_response_date_element() ->
    {responseDate, oai_utils:datetime_to_oai_datestamp(erlang:universaltime())}.

%%%--------------------------------------------------------------------
%%% @private
%%% @doc
%%% Generate element of response which contains info about request.
%%% @end
%%%--------------------------------------------------------------------
-spec generate_request_element(cowboy_req:req()) -> {request, binary(), [proplists:property()]}.
generate_request_element(Req) ->
    generate_request_element([], Req).

%%%--------------------------------------------------------------------
%%% @private
%%% @doc
%%% Generate element of response which contains info about request.
%%% @end
%%%--------------------------------------------------------------------
-spec generate_request_element(
    ParsedArgs :: [proplists:property()], Req :: cowboy_req:req()) ->
    {request, binary(), [proplists:property()]}.
generate_request_element(ParsedArgs, Req) ->
    URL = iolist_to_binary(cowboy_req:uri(Req, #{qs => undefined})),
    {request, URL, ParsedArgs}.
