%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
-module(oai_parser).
-author("Jakub Kudzia").

%% API
-export([process_and_validate_args/1]).

process_and_validate_args(Args) ->
    try
        true = key_occurs_exactly_once(<<"verb">>, Args),
        Verb = proplists:get_value(<<"verb">>, Args),
        Module = oai_utils:verb_to_module(Verb),
        Args2 = proplists:delete(<<"verb">>, Args),
        process_and_validate_verb_specific_arguments(Module, Args2),
        case oai_utils:is_harvesting(Verb) of
            false -> ok;
            true -> parse_harvesting_arguments(Args)
        end,
        {Verb, Args2}
    catch
        throw:{missing_key, <<"verb">>}  ->
            throw({badVerb, "The verb argument is missing."});
        throw:{repeated_key, <<"verb">>}  ->
            throw({badVerb, "The verb argument is repeated."});
        throw:{not_legal_verb, BadVerb} -> throw({badVerb,
            str_utils:format("The verb argument ~s is not a legal OAI-PMH verb.",[BadVerb])});
        throw:{repeated_key, Key}  -> throw({badArgument,
            str_utils:format("The request includes repeated argument ~s.",[Key])});
        throw:{value_not_defined, Key}  -> throw({badArgument,
            str_utils:format("The request argument ~s has empty value.",[Key])});
        throw:{cannotDisseminateFormat, MetadataPrefix}  -> throw({cannotDisseminateFormat,
            str_utils:format(
                "The metadata format identified by the value ~s"
                "given for the metadataPrefix argument is not "
                "supported by this repository.",[MetadataPrefix])});
        throw:{missing_key, Keys}  ->
            KeysStr = [str_utils:to_list(K) || K <- Keys],
            throw({badArgument,
            str_utils:format("The request is missing required arguments: ~p.",[KeysStr])});
        throw:exclusive_argument  -> throw({badArgument,
            "Exclusive argument is not an only argument"});
        throw:{illegal_argument, IllegalArgs} ->
            IllegalArgsStr = [str_utils:to_list(A) || A <- IllegalArgs],
            throw({badArgument,
            str_utils:format("The request includes illegal arguments: ~p.", [IllegalArgsStr])});
        throw:set_not_supported -> throw(noSetHierarchy);
        throw:{granularity_mismatch, From, Until} -> throw({badArgument,
            str_utils:format("Datestamps from=~s and until=~s "
                             "have different granularity.", [From, Until])});
        throw:{invalid_date_format, Date} -> throw({badArgument,
            str_utils:format("Datestamp ~s has invalid format.", [Date])});
        throw:{wrong_datestamps_relation, From, Until} ->throw({badArgument,
            str_utils:format("Datestamp from=~s is greater than until=~s: ", [From, Until])})
    end
.

%%%===================================================================
%%% Internal functions
%%%===================================================================


process_and_validate_verb_specific_arguments(Module, Args) ->
    all_keys_occur_exactly_once(Args),
    parse_exclusive_arguments(Module, Args),
    parse_required_arguments(Module, Args),
    illegal_arguments_do_not_exist(Module, Args),
    args_values_are_well_defined(Args).

parse_required_arguments(Module, ArgsList) ->
    RequiredArgumentsSet = sets:from_list(Module:required_arguments()),
    ExistingArgumentsSet = sets:from_list(proplists:get_keys(ArgsList)),
    case sets:is_subset(RequiredArgumentsSet, ExistingArgumentsSet) of
        true -> ok;
        false ->
            MissingArgs = sets:to_list(sets:subtract(RequiredArgumentsSet, ExistingArgumentsSet)),
            throw({missing_key, MissingArgs})
    end.

parse_exclusive_arguments(Module, ArgsList) ->
    ExclusiveArgumentsSet = sets:from_list(Module:exclusive_arguments()),
    ExistingArgumentsSet = sets:from_list(proplists:get_keys(ArgsList)),
    case sets:is_disjoint(ExclusiveArgumentsSet, ExistingArgumentsSet) of
        true -> ok;
        false ->
            case ExclusiveArgumentsSet == ExistingArgumentsSet of
                true -> ok;
                false -> throw(exclusive_argument)
            end
    end.

parse_harvesting_arguments(ArgsList) ->
    case proplists:get_value(<<"set">>, ArgsList) of
        undefined ->
            parse_harvesting_metadata_prefix(ArgsList),
            parse_harvesting_datestamps(ArgsList);
        _ ->
            throw(noSetHierarchy)
    end.

parse_harvesting_metadata_prefix(ArgsList) ->
    MetadataPrefix = proplists:get_value(<<"metadataPrefix">>, ArgsList),
    case lists:member(MetadataPrefix, metadata_formats:supported_formats()) of
        false -> throw({cannotDisseminateFormat, MetadataPrefix});
        _ -> ok
    end.

parse_harvesting_datestamps(ArgsList) ->
    From = proplists:get_value(<<"from">>, ArgsList),
    Until = proplists:get_value(<<"until">>, ArgsList),
    case validate_datestamps_format(From, Until) of
        {undefined, _} -> ok;
        {_, undefined} -> ok;
        {Date1, Date2} ->
            case oai_utils:dates_have_the_same_granularity(Date1, Date2) of
                false ->
                    throw({granularity_mismatch, From, Until});
                true ->
                    case oai_utils:is_earlier_or_equal(Date1, Date2) of
                        true -> ok;
                        false -> throw({wrong_datestamps_relation, From, Until})
                    end
            end
    end.

validate_datestamps_format(Date1, Date2) ->
    {validate_datestamp_format(Date1), validate_datestamp_format(Date2)}.

validate_datestamp_format(undefined) -> undefined;
validate_datestamp_format(Date) ->
    case oai_utils:oai_datestamp_to_datetime(Date) of
        {error, invalid_date_format} ->throw({invalid_date_format, Date});
        ConvertedDate ->
            case is_valid_datestamp(ConvertedDate) of
                true -> ConvertedDate;
                false -> throw({invalid_date_format, Date})
            end
    end.

is_valid_datestamp(Date = {_Y, _M, _D}) -> calendar:valid_date(Date);
is_valid_datestamp({Date = {_, _, _}, Time = {_H, _Min, _S}}) ->
    is_valid_datestamp(Date) and is_valid_time(Time);
is_valid_datestamp(_) ->
    throw(invalid_date_format).

is_valid_time({H, M, S}) ->
    (0 =< H) and (H < 24) and
    (0 =< M) and (M < 60) and
    (0 =< S) and (S < 60) .

illegal_arguments_do_not_exist(Module, ArgsList) ->
    KnownArgumentsSet = sets:from_list(Module:required_arguments() ++
        Module:optional_arguments() ++
        Module:exclusive_arguments()),
    ExistingArgumentsSet = sets:from_list(proplists:get_keys(ArgsList)),
    case sets:is_subset(ExistingArgumentsSet, KnownArgumentsSet) of
        true -> ok;
        false ->
            IllegalArgs = sets:to_list(sets:subtract(ExistingArgumentsSet, KnownArgumentsSet)),
            throw({illegal_argument, IllegalArgs})
    end.

all_keys_occur_exactly_once(Proplist) ->
    lists:foldl(fun(K, Acc) ->
        key_occurs_exactly_once(K, Proplist) and Acc
    end, true, proplists:get_keys(Proplist)).

args_values_are_well_defined(Args) ->
    lists:foreach(fun({K, V}) ->
        case V of
            true -> throw({value_not_defined, K});
            _ -> ok
        end
    end, Args).


%%key exists and is not duplicated
key_occurs_exactly_once(Key, Proplist) ->
    case count_key_occurrences(Key, Proplist) of
        0 -> throw({missing_key, Key});
        1 -> true;
        _ -> throw({repeated_key, Key})
    end.


count_key_occurrences(Key, Proplist) ->
    lists:foldl(fun({K, _V}, Sum) ->
        case K of
            Key -> 1 + Sum;
            _ -> Sum
        end
    end, 0, Proplist).



