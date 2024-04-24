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
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([serialize_datestamp/1, deserialize_datestamp/1, is_harvesting/1,
    verb_to_module/1, is_earlier_or_equal/2, dates_have_the_same_granularity/2,
    to_xml/1, encode_xml/1, ensure_list/1,
    request_arguments_to_handle_listing_opts/1, harvest/2, oai_identifier_decode/1,
    build_oai_header/3, build_oai_record/3, build_oai_record/4
]).
-export([get_handle/1]).


%%%--------------------------------------------------------------------
%%% @doc
%%% Decode handle id from oai identifier supported by oz-worker.
%%% oai identifier is in form:
%%%     oai:<onezone-domain>:<handle_id>
%%% @end
%%%--------------------------------------------------------------------
-spec oai_identifier_decode(oai_id()) -> od_handle:id().
oai_identifier_decode(OAIId) ->
    Prefix = ?OAI_IDENTIFIER_PREFIX,
    PrefixSize = size(Prefix),
    case binary:match(OAIId, Prefix) of
        {0, PrefixSize} ->
            binary:part(OAIId, size(OAIId), PrefixSize - size(OAIId));
        _ ->
            throw({illegalId, OAIId})
    end.

-spec build_oai_header(od_handle:timestamp_seconds(), od_handle_service:id(),
    od_handle:id()) -> #oai_header{}.
build_oai_header(TimeSeconds, HandleServiceId, HandleId) ->
    OaiId = oai_identifier_encode(HandleId),
    #oai_header{
        identifier = OaiId,
        datestamp = serialize_datestamp(time:seconds_to_datetime(TimeSeconds)),
        set_spec = HandleServiceId
    }.

% TODO VFS-11924 https://git.onedata.org/projects/VFS/repos/oz-worker/pull-requests/1591/overview?commentId=99463
-spec build_oai_record(od_handle:timestamp_seconds(), od_handle_service:id(),
    od_handle:id()) -> #oai_record{}.
build_oai_record(TimeSeconds, HandleServiceId, HandleId) ->
    Handle = get_handle(HandleId),
    build_oai_record(TimeSeconds, HandleServiceId, HandleId, Handle).

-spec build_oai_record(od_handle:timestamp_seconds(), od_handle_service:id(),
    od_handle:id(), #od_handle{}) -> #oai_record{}.
build_oai_record(TimeSeconds, HandleServiceId, HandleId, Handle) ->
    MetadataPrefix = Handle#od_handle.metadata_prefix,
    #oai_record{
        header = build_oai_header(TimeSeconds, HandleServiceId, HandleId),
        metadata = #oai_metadata{
            metadata_prefix = MetadataPrefix,
            raw_value = Handle#od_handle.metadata,
            handle = Handle
        }
    }.

%%%--------------------------------------------------------------------
%%% @equiv time:datetime_to_iso8601(DateTime).
%%%--------------------------------------------------------------------
-spec serialize_datestamp(calendar:datetime()) -> binary().
serialize_datestamp(DateTime) ->
    time:datetime_to_iso8601(DateTime).

%%%--------------------------------------------------------------------
%%% @doc
%%% Parses a datestamp into erlang's calendar:datetime() or calendar:date()
%%% format, depending on the input granularity.
%%%
%%% Excerpt from the OAI PMH specification:
%%% These arguments support the "Complete date" and the "Complete date plus hours,
%%% minutes and seconds" granularities defined in ISO8601. The legitimate formats
%%% are YYYY-MM-DD and YYYY-MM-DDThh:mm:ssZ. Both arguments must have the same
%%% granularity.
%%% @end
%%%--------------------------------------------------------------------
-spec deserialize_datestamp(undefined | binary()) ->
    undefined | maybe_invalid_datestamp() | {error, invalid_date_format}.
deserialize_datestamp(undefined) ->
    undefined;
deserialize_datestamp(Datestamp) ->
    try
        case byte_size(Datestamp) of
            10 ->
                {Date, _Time} = time:iso8601_to_datetime(Datestamp),
                Date;
            20 ->
                time:iso8601_to_datetime(Datestamp);
            _ ->
                {error, invalid_date_format}
        end
    catch _:_ ->
        {error, invalid_date_format}
    end.


-spec request_arguments_to_handle_listing_opts([proplists:property()]) -> handles:listing_opts().
request_arguments_to_handle_listing_opts(Args) ->
    case proplists:get_value(<<"resumptionToken">>, Args) of
        undefined ->
            #{
                metadata_prefix => proplists:get_value(<<"metadataPrefix">>, Args),
                service_id => proplists:get_value(<<"set">>, Args, undefined),
                from => utils:convert_defined(
                    deserialize_datestamp(proplists:get_value(<<"from">>, Args, undefined)),
                    fun time:datetime_to_seconds/1
                ),
                until => utils:convert_defined(
                    deserialize_datestamp(proplists:get_value(<<"until">>, Args, undefined)),
                    fun time:datetime_to_seconds/1
                )
            };
        ResumptionToken ->
            #{
                resumption_token => ResumptionToken
            }
    end.

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
-spec harvest(handles:listing_opts(), function()) -> oai_response().
harvest(ListingOpts, HarvestingFun) ->
    {Identifiers, NewResumptionToken} = handles:list(ListingOpts),
    HarvestedMetadata = lists:map(fun({TimeSeconds, HandleServiceId, HandleId}) ->
        HarvestingFun(TimeSeconds, HandleServiceId, HandleId) end, Identifiers),

    % TODO VFS-11906 consider a situation when a resumption token has been returned because
    % there is still one entry to be listed, but in the meantime it is deleted - then, the listing
    % may return an empty result with a token and the code below will crash
    % this however cannot happen if we support deletions (report deleted handles) in OAI-PMH
    case HarvestedMetadata of
        [] ->
            MetadataPrefix = maps:get(metadata_prefix, ListingOpts),
            SetSpec = maps:get(service_id, ListingOpts),
            FromDatestamp = case maps:get(from, ListingOpts) of
                undefined -> undefined;
                From -> serialize_datestamp(time:seconds_to_datetime(From))
            end,
            UntilDatestamp = case maps:get(until, ListingOpts) of
                undefined -> undefined;
                Until -> serialize_datestamp(time:seconds_to_datetime(Until))
            end,
            throw({noRecordsMatch, FromDatestamp, UntilDatestamp, SetSpec, MetadataPrefix});
        _ ->
            #oai_listing_result{
                batch = HarvestedMetadata,
                % According to OAI-PMH spec, the token MUST be present in the response
                % if an incomplete list is returned, and MUST be present and MUST be empty (<<>>)
                % when the last batch that completes the list is returned.
                % However, if the whole list is returned in one response,
                % there should be no resumption token element at all (undefined).
                resumption_token = case {NewResumptionToken, ListingOpts} of
                    {undefined, #{resumption_token := _}} -> <<>>;
                    {undefined, _} -> undefined;
                    {Binary, _} when is_binary(Binary) -> Binary
                end
            }
    end.


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
    time:datetime_to_seconds(D1) =< time:datetime_to_seconds(D2).

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
to_xml([]) -> [];
to_xml({_Name, undefined}) -> [];
to_xml(#xmlElement{} = XML) -> XML;
to_xml({_Name, Record = #oai_record{}}) -> to_xml(Record);
to_xml({_Name, Header = #oai_header{}}) -> to_xml(Header);
to_xml({_Name, MetadataFormat = #oai_metadata_format{}}) -> to_xml(MetadataFormat);
to_xml({_Name, Set = #oai_set{}}) -> to_xml(Set);
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
to_xml(#oai_header{identifier = Identifier, datestamp = Datestamp, set_spec = SetSpec}) ->
    #xmlElement{
        name = header,
        content = lists:flatten([
            ensure_list(to_xml({identifier, Identifier})),
            ensure_list(to_xml({datestamp, Datestamp})),
            ensure_list(to_xml({setSpec, SetSpec}))
        ])
    };
to_xml(#oai_metadata{metadata_prefix = MetadataPrefix, raw_value = RawValue, handle = Handle}) ->
    ParsedMetadata = case oai_metadata:parse_xml(RawValue) of
        {ok, Parsed} ->
            Parsed;
        error ->
            % @TODO VFS-11906 Temporary workaround, rework
            % @TODO VFS-11906 Consider adding tests for this edge case
            EmptyMetadata = case MetadataPrefix of
                ?OAI_DC_METADATA_PREFIX ->
                    #xmlElement{name = metadata, content = []};
                ?EDM_METADATA_PREFIX ->
                    #xmlElement{name = 'rdf:RDF', content = []}
            end,
            {ok, RevisedMetadata} = oai_metadata:revise_for_publication(
                MetadataPrefix,
                EmptyMetadata,
                Handle#od_handle.resource_id,
                ?check(share_logic:get(?ROOT, Handle#od_handle.resource_id))
            ),
            oai_metadata:insert_public_handle(MetadataPrefix, RevisedMetadata, Handle#od_handle.public_handle)
        % @TODO VFS-11906 This might be useful when we upgrade the models and ensure there are valid xmls in the DB
%%            % this should theoretically never happen as the metadata is sanitized before
%%            % being written to the DB, so let it crash in case of unforeseen errors
%%            error({bad_metadata, RawValue})
    end,

    #xmlElement{
        name = metadata,
        content = ensure_list(to_xml(oai_metadata:adapt_for_oai_pmh(MetadataPrefix, ParsedMetadata)))
    };
to_xml(#oai_metadata_format{metadataPrefix = MetadataPrefix, schema = Schema,
    metadataNamespace = Namespace}) ->
    #xmlElement{
        name = metadataFormat,
        content = ensure_list(to_xml({metadataPrefix, MetadataPrefix})) ++
            ensure_list(to_xml({schema, Schema})) ++
            ensure_list(to_xml({metadataNamespace, Namespace}))
    };
to_xml(#oai_set{set_spec = SetSpec, set_name = SetName}) ->
    #xmlElement{
        name = set,
        content = lists:flatten([
            ensure_list(to_xml({setSpec, SetSpec})),
            ensure_list(to_xml({setName, SetName}))
        ])
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
        content = ensure_list(to_xml(Content)),
        attributes = lists:map(fun({N, V}) ->
            #xmlAttribute{name = ensure_atom(N), value = str_utils:to_list(V)}
        end, Attributes)
    };
to_xml(Content) when is_binary(Content) ->
    #xmlText{value = str_utils:to_list(Content)}.


-spec encode_xml(#xmlElement{}) -> binary().
encode_xml(Xml) ->
    % NOTE: xmerl scans strings in UTF8 (essentially the result of binary_to_list(<<_/utf8>>),
    % but exports as a unicode erlang string
    str_utils:unicode_list_to_binary(xmerl:export_simple([Xml], xmerl_xml, [
        {prolog, ["<?xml version=\"1.0\" encoding=\"utf-8\" ?>"]}
    ])).


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
        -> undefined | calendar:datetime().
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

%%%--------------------------------------------------------------------
%%% @private
%%% @doc
%%% Encode handle id to oai identifier supported by oz-worker.
%%% oai identifier is in form:
%%%     oai:<onezone-domain>:<handle_id>
%%% @end
%%%--------------------------------------------------------------------
-spec oai_identifier_encode(od_handle:id()) -> oai_id().
oai_identifier_encode(Id) ->
    <<?OAI_IDENTIFIER_PREFIX/binary, Id/binary>>.