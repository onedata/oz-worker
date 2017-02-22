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

-include("entity_logic.hrl").
-include("http/handlers/oai.hrl").
-include("http/handlers/oai_errors.hrl").
-include("datastore/oz_datastore_models_def.hrl").

%% API
-export([datetime_to_oai_datestamp/1, oai_datestamp_to_datetime/1,
    is_harvesting/1, verb_to_module/1, is_earlier_or_equal/2,
    dates_have_the_same_granularity/2, to_xml/1, ensure_list/1, harvest/4,
    oai_identifier_encode/1, oai_identifier_decode/1]).
-export([list_handles/0, get_handle/1]).

%%%--------------------------------------------------------------------
%%% @doc
%%% Encode handle id to oai identifier supported by oz-worker.
%%% oai identifier is in form:
%%%     oai:onedata.org:<handle_id>
%%% @end
%%%--------------------------------------------------------------------
-spec oai_identifier_encode(od_handle:id()) -> oai_id().
oai_identifier_encode(Id) ->
    <<?OAI_IDENTIFIER_PREFIX/binary, Id/binary>>.

%%%--------------------------------------------------------------------
%%% @doc
%%% Decode handle id from oai identifier supported by oz-worker.
%%% oai identifier is in form:
%%%     oai:onedata.org:<handle_id>
%%% @end
%%%--------------------------------------------------------------------
-spec oai_identifier_decode(oai_id()) -> od_handle:id().
oai_identifier_decode(OAIId) ->
    PrefixSize = size(?OAI_IDENTIFIER_PREFIX),
    case binary:match(OAIId, ?OAI_IDENTIFIER_PREFIX) of
        {0, PrefixSize} ->
            binary:part(OAIId, size(OAIId), PrefixSize - size(OAIId));
        _ ->
            throw({illegalId, OAIId})
    end.

%%%--------------------------------------------------------------------
%%% @equiv timestamp_utils:datetime_to_datestamp(DateTime).
%%%--------------------------------------------------------------------
-spec datetime_to_oai_datestamp(DateTime :: erlang:datetime()) -> binary().
datetime_to_oai_datestamp(DateTime) ->
    timestamp_utils:datetime_to_datestamp(DateTime).

%%%--------------------------------------------------------------------
%%% @equiv timestamp_utils:datestamp_to_datetime(Datestamp).
%%%--------------------------------------------------------------------
-spec oai_datestamp_to_datetime(undefined | binary()) -> maybe_invalid_datestamp().
oai_datestamp_to_datetime(Datestamp) ->
    timestamp_utils:datestamp_to_datetime(Datestamp).

%%%--------------------------------------------------------------------
%%% @doc
%%% Function responsible for performing harvesting.
%%% Harvests metadata which has representation in MetadataPrefix format
%%% and which has Datestamp in range [FromDatestamp, UntilDatestamp].
%%% HarvestingFun must be a callback which takes Identifier and Handle record
%%% and returns intended part of metadata.
%%% Throws with noRecordsMatch if nothing is harvested.
%%% @end
%%%--------------------------------------------------------------------
-spec harvest(binary(), binary(), binary(), function()) -> [term()].
harvest(MetadataPrefix, FromDatestamp, UntilDatestamp, HarvestingFun) ->
    From = oai_datestamp_to_datetime(FromDatestamp),
    Until = oai_datestamp_to_datetime(UntilDatestamp),
    Identifiers = list_handles(),
    HarvestedMetadata = lists:flatmap(fun(Identifier) ->
        Handle = get_handle(Identifier),
        case should_be_harvested(From, Until, MetadataPrefix, Handle) of
            false -> [];
            true ->
                [HarvestingFun(Identifier, Handle)]
        end
    end, Identifiers),
    case HarvestedMetadata of
        [] ->
            throw({noRecordsMatch, FromDatestamp, UntilDatestamp, MetadataPrefix});
        _ -> HarvestedMetadata
    end.

%%%--------------------------------------------------------------------
%%% @doc
%%% Function returns true if metadata of record identified by Identifier
%%% should be harvested with given harvesting conditions (From, Until,
%%% MetadataPrefix).
%%% @end
%%%--------------------------------------------------------------------
-spec should_be_harvested(supported_datestamp(), supported_datestamp(),
    binary(), #od_handle{}) -> boolean().
should_be_harvested(_From, _Until, _MetadataPrefix, #od_handle{metadata = undefined}) ->
    false;
should_be_harvested(From, Until, MetadataPrefix, #od_handle{timestamp = Datestamp}) ->
    MetadataFormats = metadata_formats:supported_formats(),
    is_in_time_range(From, Until, Datestamp) and
        lists:member(MetadataPrefix, MetadataFormats).

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
        content = [#xmlText{value = str_utils:to_list(Description)}]
    };
to_xml(#oai_record{header = Header, metadata = Metadata, about = About}) ->
    #xmlElement{
        name = record,
        content = ensure_list(to_xml(Header)) ++
            ensure_list(to_xml(Metadata)) ++
            ensure_list(to_xml(About))
    };
to_xml(#oai_header{identifier = Identifier, datestamp = Datestamp}) ->
    #xmlElement{
        name = header,
        content = ensure_list(to_xml({identifier, Identifier})) ++
        ensure_list(to_xml({datestamp, Datestamp}))};
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
to_xml(#oai_about{value = Value}) ->
    #xmlElement{
        name = about,
        content = ensure_list(to_xml(Value))
    };
to_xml({Name, Content}) ->
    to_xml({Name, Content, []});
to_xml([{Name, Content}]) ->
    [to_xml({Name, Content})];
to_xml([{Name, Content} | Rest]) ->
    [to_xml({Name, Content}) | ensure_list(to_xml(Rest))];
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
to_xml(Content) -> #xmlText{value = str_utils:to_list(Content)}.


%%%-------------------------------------------------------------------
%%% @doc
%%% Ensure Arg is list.
%%% @end
%%%-------------------------------------------------------------------
-spec ensure_list(term()) -> list().
ensure_list(undefined) -> [];
ensure_list(Arg) when is_list(Arg) -> Arg;
ensure_list(Arg) -> [Arg].


%%%-------------------------------------------------------------------
%%% @doc
%%% Returns the list of all handles in the system.
%%% @end
%%%-------------------------------------------------------------------
-spec list_handles() -> [od_handle:id()].
list_handles() ->
    {ok, HandlesList} = handle_logic:list(?ROOT),
    HandlesList.


%%%-------------------------------------------------------------------
%%% @doc
%%% Retrieves specified handle record.
%%% @end
%%%-------------------------------------------------------------------
-spec get_handle(HandleId :: od_handle:id()) -> #od_handle{}.
get_handle(HandleId) ->
    {ok, Handle} = handle_logic:get(?ROOT, HandleId),
    Handle.


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
-spec granularity_days_to_seconds({Direction :: max | min, supported_datestamp()})
        -> undefined | erlang:datetime().
granularity_days_to_seconds({_, undefined}) -> undefined;
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