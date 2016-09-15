%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
-module(oai_utils).
-author("Jakub Kudzia").

-include("http/handlers/oai.hrl").
-include("http/handlers/oai_errors.hrl").

%% API
-export([datetime_to_oai_datestamp/1, oai_datestamp_to_datetime/1,
    is_harvesting/1, verb_to_module/1, is_earlier_or_equal/2,
    dates_have_the_same_granularity/2, to_xml/1, ensure_list/1,
    harvest/2, harvest/4, harvest/3]).

-define(DATESTAMP_REGEX,
    "(\\d{4})-(\\d{2})-(\\d{2})(?:$|(?:T(\\d{2}):(\\d{2}):(\\d{2})Z){1})").


datetime_to_oai_datestamp(DateTime) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = DateTime,
    str_utils:format_bin(
        "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
        [Year, Month, Day, Hour, Minute, Second]).

oai_datestamp_to_datetime(undefined) -> undefined;
oai_datestamp_to_datetime(Datestamp) ->
    {ok, Regex} = re:compile(?DATESTAMP_REGEX),
    case re:run(Datestamp, Regex, [{capture, all_but_first, list}]) of
        {match, Matched} ->
            case [list_to_integer(E) || E <- Matched] of
                [Y, M, D, H, Min, S] ->
                    {{Y, M, D},{H, Min, S}};
                [Y, M, D] ->
                    {Y, M, D};
                _ -> {error, invalid_date_format}
            end;
        nomatch -> {error, invalid_date_format}
    end.

is_harvesting(<<"ListIdentifiers">>) -> true;
is_harvesting(<<"ListRecords">>) -> true;
is_harvesting(<<"ListSets">>) -> true;
is_harvesting(_) -> false.

harvest(Identifier, MetadataPrefix) ->
    harvest(Identifier, MetadataPrefix, undefined, undefined).

harvest(MetadataPrefix, From, Until) ->
    harvest(all, MetadataPrefix, From, Until).

harvest(Identifier, MetadataPrefix, FromDatestamp, UntilDatestamp) ->
    From = oai_datestamp_to_datetime(FromDatestamp),
    Until = oai_datestamp_to_datetime(UntilDatestamp),
    ShareIds = case Identifier of
        all ->
            {ok, Ids} = share_logic:list(),
            Ids;
        Id -> [Id]
    end,
    lists:filter(fun(ShareId) ->
        {ok, MetadataInfo} = share_logic:get_metadata(ShareId),
        try proplists:get_value(<<"metadata">>, MetadataInfo) of
            undefined -> false;
            _ ->
                Datestamp = proplists:get_value(<<"metadata_timestamp">>, MetadataInfo),
                MetadataFormats = proplists:get_value(<<"metadata_formats">>, MetadataInfo),
                is_earlier_or_equal(From, Datestamp) and
                is_earlier_or_equal(Datestamp, Until) and
                is_requested_format_available(MetadataPrefix, MetadataFormats)
        catch
            _:_ -> false
        end
    end, ShareIds).

is_earlier_or_equal(undefined, _Date) -> true;
is_earlier_or_equal(_Date, undefined) -> true;
is_earlier_or_equal(Date1, Date2) ->
    D1 = granularity_days_to_seconds({floor, Date1}),
    D2 = granularity_days_to_seconds({ceiling, Date2}),
    calendar:datetime_to_gregorian_seconds(D1) =<
        calendar:datetime_to_gregorian_seconds(D2).

dates_have_the_same_granularity(Date1, Date2) ->
    case {granularity(Date1), granularity(Date2)} of
        {Granularity, Granularity} ->true;
        {_Granularity, _OtherGranularity} -> false
    end.

-spec verb_to_module(binary()) -> oai_verb().
verb_to_module(<<"Identify">>) -> identify;
verb_to_module(<<"GetRecord">>) -> get_record;
verb_to_module(<<"ListIdentifiers">>) -> list_identifiers;
verb_to_module(<<"ListMetadataFormats">>) -> list_metadata_formats;
verb_to_module(<<"ListRecords">>) -> list_records;
verb_to_module(<<"ListSets">>) -> list_sets;
verb_to_module(Verb) -> throw({not_legal_verb, Verb}).


to_xml(undefined) -> [];
to_xml({_Name, undefined}) -> [];
to_xml(#xmlElement{}=XML) -> XML;
to_xml({_Name, Record=#oai_record{}}) -> to_xml(Record);
to_xml({_Name, Header=#oai_header{}}) -> to_xml(Header);
to_xml({_Name, MetadataFormat=#oai_metadata_format{}}) -> to_xml(MetadataFormat);
to_xml(#oai_error{code=Code, description=Description}) ->
    #xmlElement{
        name=error,
        attributes = [#xmlAttribute{name=code, value=Code}],
        content = [Description]
    };
to_xml(#oai_record{header = Header, metadata = Metadata, about=About}) ->
    #xmlElement{
        name = record,
        content = ensure_list(to_xml(Header)) ++
                  ensure_list(to_xml(Metadata)) ++
                  ensure_list(to_xml(About))
    };
to_xml(#oai_header{identifier = Identifier, datestamp = Datestamp, setSpec = SetSpec}) ->
    #xmlElement{
        name = header,
        content = ensure_list(to_xml({identifier, Identifier})) ++
                  ensure_list(to_xml({datestamp, Datestamp})) ++
                  ensure_list(to_xml({setSpec, SetSpec}))};
to_xml(#oai_metadata{metadata_format=Format, value=Value}) ->
    MetadataPrefix = Format#oai_metadata_format.metadataPrefix,
    Mod = metadata_formats:module(MetadataPrefix),
    #xmlElement{
        name=metadata,
        content=ensure_list(to_xml(Mod:encode(Value)))};
to_xml(#oai_metadata_format{metadataPrefix = MetadataPrefix, schema=Schema,
        metadataNamespace = Namespace }) ->

    #xmlElement{
        name=metadataFormat,
        content = ensure_list(to_xml({metadatPrefix, MetadataPrefix})) ++
                  ensure_list(to_xml({schema, Schema})) ++
                  ensure_list(to_xml({metadataNamespace, Namespace}))
    };
to_xml({Name, Content}) ->
    to_xml({Name, Content, []});
to_xml([{Name, Content}]) ->
    [to_xml({Name, Content})];
to_xml([{Name, Content} | Rest]) ->
    [to_xml({Name, Content}) | to_xml(Rest)];
to_xml({Name, ContentList = [{_,_} | _], Attributes}) ->
    #xmlElement{
        name=ensure_atom(Name),
        content = to_xml(ContentList),
        attributes = lists:map(fun({N, V}) ->
            #xmlAttribute{name=ensure_atom(N), value = str_utils:to_list(V)}
        end, Attributes)
    };
to_xml({Name, Content, Attributes}) ->
    #xmlElement{
        name=ensure_atom(Name),
        content = [to_xml(Content)],
        attributes = lists:map(fun({N, V}) ->
            #xmlAttribute{name=ensure_atom(N), value = str_utils:to_list(V)}
        end, Attributes)
    };
to_xml(Content) -> str_utils:to_list(Content).

ensure_list(undefined) -> [];
ensure_list(Arg) when is_list(Arg) -> Arg;
ensure_list(Arg) -> [Arg].

%%%===================================================================
%%% Internal functions
%%%===================================================================

granularity({_, _, _}) -> day_granularity;
granularity({{_, _, _}, {_, _, _}}) -> seconds_granularity.

granularity_days_to_seconds(undefined) -> undefined;
granularity_days_to_seconds({floor, {Y, M, D}}) -> {{Y, M, D}, {0, 0, 0}};
granularity_days_to_seconds({ceiling, {Y, M, D}}) -> {{Y, M, D}, {23, 59, 59}};
granularity_days_to_seconds({floor, {{Y, M, D}, {H, Min,S}}}) -> {{Y, M, D}, {H, Min,S}};
granularity_days_to_seconds({ceiling, {{Y, M, D}, {H, Min,S}}}) -> {{Y, M, D}, {H, Min,S}}.


ensure_atom(Arg) when is_atom(Arg) -> Arg;
ensure_atom(Arg) when is_binary(Arg) -> binary_to_atom(Arg, latin1);
ensure_atom(Arg) when is_list(Arg) -> list_to_atom(Arg).

is_requested_format_available(MetadataPrefix, SupportedMetadataFormats) ->
    lists:member(MetadataPrefix, SupportedMetadataFormats).