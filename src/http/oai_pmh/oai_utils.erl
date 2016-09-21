%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% This module contains utility functions used in modules handling
%%% OAI-PMH protocol.
%%% @end
%%%-------------------------------------------------------------------
-module(oai_utils).
-author("Jakub Kudzia").

-include("http/handlers/oai.hrl").
-include("http/handlers/oai_errors.hrl").

%% API
-export([datetime_to_oai_datestamp/1, oai_datestamp_to_datetime/1,
    is_harvesting/1, verb_to_module/1, is_earlier_or_equal/2,
    dates_have_the_same_granularity/2, to_xml/1, ensure_list/1, harvest/4]).


%% Macro with regex matching allowed datestamps
%%  * YYYY-MM-DDThh:mm:ssZ
%%  * YYYY-MM-DD
-define(DATESTAMP_REGEX,
    "(\\d{4})-(\\d{2})-(\\d{2})(?:$|(?:T(\\d{2}):(\\d{2}):(\\d{2})Z){1})").

%%%--------------------------------------------------------------------
%%% @doc
%%% Converts DateTime to format accepted by OAI-PMH which is in form
%%  YYYY-MM-DDThh:mm:ssZ
%%% @end
%%%--------------------------------------------------------------------
-spec datetime_to_oai_datestamp(DateTime :: erlang:datetime()) -> binary().
datetime_to_oai_datestamp(DateTime) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = DateTime,
    str_utils:format_bin(
        "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
        [Year, Month, Day, Hour, Minute, Second]).

%%%--------------------------------------------------------------------
%%% @doc
%%% Converts datestamp from format defined by OAI-PMH to
%%% erlang:datetime() or erlang:date().
%%% Converts:
%%%     * YYYY-MM-DDT:hh:mm:ssZ to {{Year, Month, Day},{Hour, Minutes, Seconds}}
%%%     * YYYY-MM-DD to {Year, Month, Day}
%%% @end
%%%--------------------------------------------------------------------
-spec oai_datestamp_to_datetime(undefined | binary()) -> undefined | supported_datestamp().
oai_datestamp_to_datetime(undefined) -> undefined;
oai_datestamp_to_datetime(Datestamp) ->
    {ok, Regex} = re:compile(?DATESTAMP_REGEX),
    case re:run(Datestamp, Regex, [{capture, all_but_first, list}]) of
        {match, Matched} ->
            case [list_to_integer(E) || E <- Matched] of
                [Y, M, D, H, Min, S] ->
                    {{Y, M, D}, {H, Min, S}};
                [Y, M, D] ->
                    {Y, M, D};
                _ -> {error, invalid_date_format}
            end;
        nomatch -> {error, invalid_date_format}
    end.

%%%--------------------------------------------------------------------
%%% @doc
%%% Function responsible for performing harvesting.
%%% Harvests metadata which has representation in MetadataPrefix format
%%% and which has Datestamp in range [FromDatestamp, UntilDatestamp].
%%% HarvestingFun must be a callback which takes Identifier and Metadata
%%% and returns intended part of metadata.
%%% Throws with noRecordsMatch if nothing is harvested.
%%% @end
%%%--------------------------------------------------------------------
-spec harvest(binary(), binary(), binary(), function()) -> [term()].
harvest(MetadataPrefix, FromDatestamp, UntilDatestamp, HarvestingFun) ->
    From = oai_datestamp_to_datetime(FromDatestamp),
    Until = oai_datestamp_to_datetime(UntilDatestamp),
    {ok, Identifiers} = share_logic:list(),
    HarvestedMetadata = lists:flatmap(fun(Identifier) ->
        {ok, Metadata} = share_logic:get_metadata(Identifier),
        case should_be_harvested(Identifier, From, Until, MetadataPrefix) of
            false -> [];
            true ->
                [HarvestingFun(Identifier, Metadata)]
        end
    end, Identifiers),
    case HarvestedMetadata of
        [] -> throw({noRecordsMatch, str_utils:format(
            "The combination of the values of the from= ~s, "
            "until= ~s and metadataPrefix= ~s arguments results "
            "in an empty list.", [From, Until, MetadataPrefix])});
        _ -> HarvestedMetadata
    end.

%%%--------------------------------------------------------------------
%%% @doc
%%% Function returns true if metadata of record identified by Identifier
%%% should be harvested with given harvesting conditions (From, Until,
%%% MetadataPrefix).
%%% @end
%%%--------------------------------------------------------------------
-spec should_be_harvested(
    binary(), supported_datestamp(), supported_datestamp(), binary()) -> boolean().
should_be_harvested(Identifier, From, Until, MetadataPrefix) ->
    {ok, Metadata} = share_logic:get_metadata(Identifier),
    case proplists:get_value(<<"metadata">>, Metadata) of
        undefined -> false;
        _ ->
            Datestamp = proplists:get_value(<<"metadata_timestamp">>, Metadata),
            MetadataFormats = proplists:get_value(<<"metadata_formats">>, Metadata),
            is_in_time_range(From, Until, Datestamp) and
                is_requested_format_available(MetadataPrefix, MetadataFormats)
    end.

%%%--------------------------------------------------------------------
%%% @doc
%%% Function returns true if Datestamp is in range [From, Until].
%%% @end
%%%--------------------------------------------------------------------
-spec is_in_time_range(
    supported_datestamp(), supported_datestamp(), supported_datestamp()) -> boolean().
is_in_time_range(From, Until, Datestamp) ->
    is_earlier_or_equal(From, Datestamp)
        and is_earlier_or_equal(Datestamp, Until).

%%%--------------------------------------------------------------------
%%% @doc
%%% Function returns true if Date1 <= Date2.
%%% @end
%%%--------------------------------------------------------------------
-spec is_earlier_or_equal(supported_datestamp(), supported_datestamp()) -> boolean().
is_earlier_or_equal(undefined, _Date) -> true;
is_earlier_or_equal(_Date, undefined) -> true;
is_earlier_or_equal(Date1, Date2) ->
    D1 = granularity_days_to_seconds({min, Date1}),
    D2 = granularity_days_to_seconds({max, Date2}),
    calendar:datetime_to_gregorian_seconds(D1) =<
        calendar:datetime_to_gregorian_seconds(D2).

%%%--------------------------------------------------------------------
%%% @doc
%%% Function returns true if Date1 and Date2 have the same granularity.
%%% @end
%%%--------------------------------------------------------------------
-spec dates_have_the_same_granularity(supported_datestamp(), supported_datestamp()) -> boolean().
dates_have_the_same_granularity(Date1, Date2) ->
    case {granularity(Date1), granularity(Date2)} of
        {Granularity, Granularity} -> true;
        {_Granularity, _OtherGranularity} -> false
    end.

%%%--------------------------------------------------------------------
%%% @doc
%%% Function maps OAI-PMH verb to module handling this verb.
%%% @end
%%%--------------------------------------------------------------------
-spec verb_to_module(binary()) -> oai_verb_module().
verb_to_module(<<"Identify">>) -> identify;
verb_to_module(<<"GetRecord">>) -> get_record;
verb_to_module(<<"ListIdentifiers">>) -> list_identifiers;
verb_to_module(<<"ListMetadataFormats">>) -> list_metadata_formats;
verb_to_module(<<"ListRecords">>) -> list_records;
verb_to_module(<<"ListSets">>) -> list_sets;
verb_to_module(Verb) -> throw({not_legal_verb, Verb}).

%%%--------------------------------------------------------------------
%%% @doc
%%% Function returns true for supported harvesting requests.
%%% @end
%%%-------------------------------------------------------------------
-spec is_harvesting(binary()) -> boolean().
is_harvesting(<<"ListIdentifiers">>) -> true;
is_harvesting(<<"ListRecords">>) -> true;
is_harvesting(_) -> false.

%%%--------------------------------------------------------------------
%%% @doc
%%% Function responsible for converting given arguments to #xmlElement{}.
%%% @end
%%%-------------------------------------------------------------------
-spec to_xml(term()) -> #xmlElement{}.
to_xml(undefined) -> [];
to_xml({_Name, undefined}) -> [];
to_xml(#xmlElement{} = XML) -> XML;
to_xml({_Name, Record = #oai_record{}}) -> to_xml(Record);
to_xml({_Name, Header = #oai_header{}}) -> to_xml(Header);
to_xml({_Name, MetadataFormat = #oai_metadata_format{}}) ->
    to_xml(MetadataFormat);
to_xml(#oai_error{code = Code, description = Description}) ->
    #xmlElement{
        name = error,
        attributes = [#xmlAttribute{name = code, value = Code}],
        content = [Description]
    };
to_xml(#oai_record{header = Header, metadata = Metadata, about = About}) ->
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
to_xml(#oai_metadata{metadata_format = Format, value = Value}) ->
    MetadataPrefix = Format#oai_metadata_format.metadataPrefix,
    Mod = metadata_formats:module(MetadataPrefix),
    #xmlElement{
        name = metadata,
        content = ensure_list(to_xml(Mod:encode(Value)))};
to_xml(#oai_metadata_format{metadataPrefix = MetadataPrefix, schema = Schema,
    metadataNamespace = Namespace}) ->
    #xmlElement{
        name = metadataFormat,
        content = ensure_list(to_xml({metadataPrefix, MetadataPrefix})) ++
            ensure_list(to_xml({schema, Schema})) ++
            ensure_list(to_xml({metadataNamespace, Namespace}))
    };
to_xml({Name, Content}) ->
    to_xml({Name, Content, []});
to_xml([{Name, Content}]) ->
    [to_xml({Name, Content})];
to_xml([{Name, Content} | Rest]) ->
    [to_xml({Name, Content}) | to_xml(Rest)];
to_xml({Name, ContentList = [{_, _} | _], Attributes}) ->
    #xmlElement{
        name = ensure_atom(Name),
        content = to_xml(ContentList),
        attributes = lists:map(fun({N, V}) ->
            #xmlAttribute{name = ensure_atom(N), value = str_utils:to_list(V)}
        end, Attributes)
    };
to_xml({Name, Content, Attributes}) ->
    #xmlElement{
        name = ensure_atom(Name),
        content = [to_xml(Content)],
        attributes = lists:map(fun({N, V}) ->
            #xmlAttribute{name = ensure_atom(N), value = str_utils:to_list(V)}
        end, Attributes)
    };
to_xml(Content) -> #xmlText{value=str_utils:to_list(Content)}.


%%%-------------------------------------------------------------------
%%% @doc
%%% Ensure Arg is list.
%%% @end
%%%-------------------------------------------------------------------
-spec ensure_list(term()) -> list().
ensure_list(undefined) -> [];
ensure_list(Arg) when is_list(Arg) -> Arg;
ensure_list(Arg) -> [Arg].

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Return date granularity.
%%% @end
%%%-------------------------------------------------------------------
-spec granularity(supported_datestamp()) -> oai_date_granularity().
granularity({_, _, _}) -> day_granularity;
granularity({{_, _, _}, {_, _, _}}) -> seconds_granularity.

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Convert datestamp with days_granularity to seconds_granularity.
%%% Depending on passed Direction argument, datestamp is converted
%%% to maximal or minimal datetime() with given date().
%%% @end
%%%-------------------------------------------------------------------
-spec granularity_days_to_seconds(
    undefined | {Direction :: max | min, supported_datestamp() }) -> undefined | erlang:datetime().
granularity_days_to_seconds(undefined) -> undefined;
granularity_days_to_seconds({min, {Y, M, D}}) -> {{Y, M, D}, {0, 0, 0}};
granularity_days_to_seconds({max, {Y, M, D}}) -> {{Y, M, D}, {23, 59, 59}};
granularity_days_to_seconds({min, {{Y, M, D}, {H, Min, S}}}) ->
    {{Y, M, D}, {H, Min, S}};
granularity_days_to_seconds({max, {{Y, M, D}, {H, Min, S}}}) ->
    {{Y, M, D}, {H, Min, S}}.

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Ensure Arg is an atom.
%%% @end
%%%-------------------------------------------------------------------
-spec ensure_atom(term()) -> atom().
ensure_atom(Arg) when is_atom(Arg) -> Arg;
ensure_atom(Arg) when is_binary(Arg) -> binary_to_atom(Arg, latin1);
ensure_atom(Arg) when is_list(Arg) -> list_to_atom(Arg).

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Returns true if MetadataPrefix is in SupportedMetadataFormats.
%%% @end
%%%-------------------------------------------------------------------
-spec is_requested_format_available(binary(), [binary()]) -> boolean().
is_requested_format_available(MetadataPrefix, SupportedMetadataFormats) ->
    lists:member(MetadataPrefix, SupportedMetadataFormats).