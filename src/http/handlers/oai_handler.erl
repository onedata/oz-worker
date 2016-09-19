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
    content_types_accepted/2, content_types_provided/2, accept_resource/2,
    provide_resource/2, generate_response/2 ]).

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

%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Return the list of content-types the resource provides.
%% @end
%%--------------------------------------------------------------------
content_types_provided(Req, State) ->
    {[{?RESPONSE_CONTENT_TYPE, provide_resource}], Req, State}.

%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Return the list of content-types the resource accepts.
%% @end
%%--------------------------------------------------------------------
content_types_accepted(Req, State) ->
    {[{?REQUEST_CONTENT_TYPE, accept_resource}], Req, State}.


%%%%--------------------------------------------------------------------
%%%% @doc Cowboy callback function.
%%%% Return whether the resource exists.
%%%% @end
%%%%--------------------------------------------------------------------
%%resource_exists(Req, State) ->
%%    %%    TODO
%%    {true, Req, State}.


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

handle_request(QueryString, Req) ->
    Response = try
        {Verb, ParsedArgs} = oai_parser:process_and_validate_args(QueryString),
        generate_response(Verb, ParsedArgs)
    catch
        throw:Error ->
            oai_errors:handle(Error)
        % TODO handle unknown errors
%%        throw:{badVerb, Description} -> ?BAD_VERB(Description);
%%        throw:badVerb -> ?BAD_VERB;
%%        throw:{badArgument, Description} -> ?BAD_ARGUMENT(Description);
%%        throw:badArgument ->;
%%        throw:noSetHierarchy -> ?NO_SET_HIERARCHY;
%%        throw:{cannotDisseminateFormat, Description} -> ?CANNOT_DISSEMINATE_FORMAT(Description);
%%        throw:{noRecordsMatch, Description} -> ?NO_RECORDS_MATCH(Description);
%%        throw:{noMetadataFromats, Description} -> ?NO_METADATA_FORMATS(Description);
%%        throw:{idDoesNotExist, Description} -> ?ID_DOES_NOT_EXIST(Description)
    end,

    RequestElement = case Response of
        #oai_error{code=badVerb} -> generate_request_element(Req);
        #oai_error{code=badArgument} -> generate_request_element(Req);
        _ -> generate_request_element(QueryString, Req)
    end,

    ResponseDate = generate_response_date_element(),

%%    io:format("Response: ~p~n", [Response]),
    ResponseXML = oai_utils:to_xml(Response),
%%    io:format("ResponseXML: ~p~n", [ResponseXML]),

%%    io:format("RequestElement: ~p~n", [RequestElement]),
    RequestElementXML = oai_utils:to_xml(RequestElement),
%%    io:format("RequestElementXML: ~p~n", [RequestElementXML]),

    ResponseDateXML = oai_utils:to_xml(ResponseDate),
%%    io:format("ResponseDateXML: ~p~n", [ResponseDateXML]),

    XML = insert_to_root_xml_element([ResponseDateXML, RequestElementXML, ResponseXML]),
%%     io:format("DEBUG::~n~p~nDEBUG~n", [XML]),
    Prolog = ["<?xml version=\"1.0\" encoding=\"utf-8\" ?>"],
    ResponseBody = xmerl:export_simple([XML], xmerl_xml, [{prolog, Prolog}]),
%%    io:format("RESPONSE: ~p~n", [lists:flatten(xmerl:export_simple([XML], xmerl_xml))]),
    Req2 = cowboy_req:set_resp_header(<<"content-type">>, ?RESPONSE_CONTENT_TYPE, Req),
    {ResponseBody, Req2}.


generate_response(Verb, Args) ->
    Module = oai_utils:verb_to_module(Verb),
%%    try
        RequiredElements = generate_required_response_elements(Module, Args),
%%        io:format("DEBUG: ~p~n", [RequiredElements]),

        OptionalElements = generate_optional_response_elements(Module, Args),
%%        io:format("DEBUG2: ~p~n", [OptionalElements]),

        {Verb, RequiredElements ++ OptionalElements}.
%%    catch
%%        throw:OAIError -> OAIError
%%    end.

generate_required_response_elements(Module, Args) ->
    lists:flatmap(fun(ElementName) ->
        case Module:get_response(ElementName, Args) of
%%            #oai_error{} = OAIError ->
%%                throw(OAIError);
            Elements when is_list(Elements)->
                [ {ElementName, Element} || Element <- Elements ];
            Element -> oai_utils:ensure_list({ElementName, Element})
        end %todo catch errors, function clause
    end, Module:required_response_elements()).

generate_optional_response_elements(Module, Args) ->
    lists:flatmap(fun(ElementName) ->
        try Module:get_response(ElementName, Args) of
            <<"">> -> [];
            [] -> [];
            Elements when is_list(Elements)->
                [ {ElementName, Element} || Element <- Elements ];
            Element -> oai_utils:ensure_list({ElementName, Element})
        catch _:_ -> []
        end
    end, Module:optional_response_elements()).

insert_to_root_xml_element(Content) when is_list(Content) ->
    ?ROOT_ELEMENT#xmlElement{content = Content};
insert_to_root_xml_element(Content) ->
    ?ROOT_ELEMENT#xmlElement{content = [Content]}.

generate_response_date_element() ->
    {responseDate, oai_utils:datetime_to_oai_datestamp(erlang:universaltime())}.

generate_request_element(Req) ->
    generate_request_element([], Req).

generate_request_element(ParsedArgs, Req) ->
    {Path, Req2} = cowboy_req:path(Req),
    {URL, _Req3} = cowboy_req:host_url(Req2),
    BaseURL = <<URL/binary, Path/binary>>,
    {request, BaseURL,  ParsedArgs}.


%% TODO
%% TODO * OAI-identifier
%% TODO * docs
%% TODO * specs
%% TODO * support resumptionToken
%% TODO * support sets
%% TODO * compression
%% TODO * identity encoding
%% TODO * allowed charset
%% TODO * define granularity as date type
%% TODO * share should have opendata attribute
%% TODO * TESTS !!!
%% TODO * get_response always returnig tuple {ElementName, OAIRsponse} ???




