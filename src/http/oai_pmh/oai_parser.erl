%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% Module responsible for parsing OAI-PMH requests.
%%% @end
%%%-------------------------------------------------------------------
-module(oai_parser).
-author("Jakub Kudzia").

-include("http/handlers/oai.hrl").

%% API
-export([process_and_validate_args/1]).

%%%-------------------------------------------------------------------
%%% @doc
%%% This function processes arguments list. It returns tuple
%%% {Verb, ArgsList} if everything is fine.
%%% Throws suitable error otherwise.
%%% @end
%%%-------------------------------------------------------------------
-spec process_and_validate_args(Args :: [proplists:property()]) -> {binary(), [proplists:property()]}.
process_and_validate_args(Args) ->
    try
        key_occurs_exactly_once(<<"verb">>, Args),
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
            throw({badVerb, <<"The verb argument is missing.">>});
        throw:{repeated_key, <<"verb">>}  ->
            throw({badVerb, <<"The verb argument is repeated.">>});
        throw:{not_legal_verb, BadVerb} -> throw({badVerb,
            str_utils:format_bin("The verb argument ~s is not a legal OAI-PMH verb.",[BadVerb])});
        throw:{repeated_key, Key}  -> throw({badArgument,
            str_utils:format_bin("The request includes repeated argument ~s.",[Key])});
        throw:{value_is_empty, Key}  -> throw({badArgument,
            str_utils:format_bin("The request argument ~s has empty value.",[Key])});
        throw:{cannotDisseminateFormat, MetadataPrefix}  -> throw({cannotDisseminateFormat,
            str_utils:format_bin(
                "The metadata format identified by the value ~s"
                "given for the metadataPrefix argument is not "
                "supported by this repository.",[MetadataPrefix])});
        throw:{missing_key, Keys}  ->
            KeysStr = [str_utils:to_list(K) || K <- Keys],
            throw({badArgument,
            str_utils:format_bin("The request is missing required arguments: ~p.",[KeysStr])});
        throw:exclusive_argument  -> throw({badArgument,
            <<"Exclusive argument is not an only argument">>});
        throw:{illegal_argument, IllegalArgs} ->
            IllegalArgsStr = [str_utils:to_list(A) || A <- IllegalArgs],
            throw({badArgument,
            str_utils:format_bin("The request includes illegal arguments: ~p.", [IllegalArgsStr])});
        throw:set_not_supported -> throw(noSetHierarchy);
        throw:{granularity_mismatch, From, Until} -> throw({badArgument,
            str_utils:format_bin("Datestamps from=~s and until=~s "
                             "have different granularity.", [From, Until])});
        throw:{invalid_date_format, Date} -> throw({badArgument,
            str_utils:format_bin("Datestamp ~s has invalid format.", [Date])});
        throw:{wrong_datestamps_relation, From, Until} ->throw({badArgument,
            str_utils:format_bin("Datestamp from=~s is greater than until=~s: ", [From, Until])})
    end
.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Process and validate arguments list for given OAI-PMH request
%%% handled by Module.
%%% Throws suitable error otherwise.
%%% @end
%%%-------------------------------------------------------------------
-spec process_and_validate_verb_specific_arguments(oai_verb_module(), [proplists:property()]) -> ok.
process_and_validate_verb_specific_arguments(Module, Args) ->
    all_keys_occur_exactly_once(Args),
    case parse_exclusive_arguments(Module, Args) of
        false -> parse_required_arguments(Module, Args);
        _ -> ok
    end,
    illegal_arguments_do_not_exist(Module, Args),
    args_values_are_not_empty(Args).

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Check if there are no repeated keys in proplist.
%%% @end
%%%-------------------------------------------------------------------
-spec all_keys_occur_exactly_once([proplists:property()]) -> boolean().
all_keys_occur_exactly_once(Proplist) ->
    lists:foldl(fun(K, Acc) ->
        key_occurs_exactly_once(K, Proplist) and Acc
    end, true, proplists:get_keys(Proplist)).

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Parse exclusive arguments for given OAI-PMH request handled by Module.
%%% If exclusive argument occurs it must be an only argument (except 'verb').
%%% Returns false if there is no exclusive argument on ArgsList, true if
%%% it's an only argument.
%%% Throws otherwise.
%%% @end
%%%-------------------------------------------------------------------
-spec parse_exclusive_arguments(oai_verb_module(), [proplists:property()]) -> boolean().
parse_exclusive_arguments(Module, ArgsList) ->
    ExclusiveArgumentsSet = sets:from_list(Module:exclusive_arguments()),
    ExistingArgumentsSet = sets:from_list(proplists:get_keys(ArgsList)),
    case sets:is_disjoint(ExclusiveArgumentsSet, ExistingArgumentsSet) of
        true -> false;
        false ->
            case ExclusiveArgumentsSet == ExistingArgumentsSet of
                true -> true;
                false -> throw(exclusive_argument)
            end
    end.

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Parse arguments that are required for given OAI-PMH request
%%% handled by Module.
%%% Throws suitable error if such argument is missing.
%%% @end
%%%-------------------------------------------------------------------
-spec parse_required_arguments(oai_verb_module(), [proplists:property()]) -> ok.
parse_required_arguments(Module, ArgsList) ->
    RequiredArgumentsSet = sets:from_list(Module:required_arguments()),
    ExistingArgumentsSet = sets:from_list(proplists:get_keys(ArgsList)),
    case sets:is_subset(RequiredArgumentsSet, ExistingArgumentsSet) of
        true -> ok;
        false ->
            MissingArgs = sets:to_list(sets:subtract(RequiredArgumentsSet, ExistingArgumentsSet)),
            throw({missing_key, MissingArgs})
    end.


%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Parse arguments for harvesting requests.
%%% Throws if argument 'set' is passed because currently repository
%%% doesn't support sets.
%%% @end
%%%-------------------------------------------------------------------
-spec parse_harvesting_arguments([proplists:property()]) -> ok.
parse_harvesting_arguments(ArgsList) ->
    case proplists:get_value(<<"set">>, ArgsList) of
        undefined ->
            parse_harvesting_metadata_prefix(ArgsList),
            parse_harvesting_datestamps(ArgsList);
        _ ->
            throw(noSetHierarchy)
    end.

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Parse harvesting metadata prefix.
%%% Throws suitable error if given metadata prefix is not supported.
%%% @end
%%%-------------------------------------------------------------------
-spec parse_harvesting_metadata_prefix([proplists:property()]) -> ok.
parse_harvesting_metadata_prefix(ArgsList) ->
    MetadataPrefix = proplists:get_value(<<"metadataPrefix">>, ArgsList),
    case lists:member(MetadataPrefix, metadata_formats:supported_formats()) of
        false -> throw({cannotDisseminateFormat, MetadataPrefix});
        _ -> ok
    end.

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Parse harvesting datestamps
%%% Throws suitable error if:
%%%     * from and until have different granularity -> granularity_mismatch
%%%     * from is later than until -> wrong_datestamps_relation
%%%     * datestamp has invalid date format -> invalid_date_format
%%% @end
%%%-------------------------------------------------------------------
-spec parse_harvesting_datestamps([proplists:property()]) -> ok.
parse_harvesting_datestamps(ArgsList) ->
    From = proplists:get_value(<<"from">>, ArgsList),
    Until = proplists:get_value(<<"until">>, ArgsList),
    case {validate_and_convert_datestamp(From), validate_and_convert_datestamp(Until)} of
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

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Validates and converts datestamp from format defined by
%%% OAI-PMH to erlang:datetime() or erlang:date().
%%% Converts:
%%%     * YYYY-MM-DDT:hh:mm:ssZ to {{Year, Month, Day},{Hour, Minutes, Seconds}}
%%%     * YYYY-MM-DD to {Year, Month, Day}
%%% @end
%%%-------------------------------------------------------------------
-spec validate_and_convert_datestamp(undefined | binary()) -> supported_datestamp().
validate_and_convert_datestamp(undefined) -> undefined;
validate_and_convert_datestamp(Date) ->
    case oai_utils:oai_datestamp_to_datetime(Date) of
        {error, invalid_date_format} ->throw({invalid_date_format, Date});
        ConvertedDate ->
            case is_valid_datestamp(ConvertedDate) of
                true -> ConvertedDate;
                false -> throw({invalid_date_format, Date})
            end
    end.

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Checks if given datestamp is valid.
%%% Throws error if datestamp is in wrong format.
%%% @end
%%%-------------------------------------------------------------------
-spec is_valid_datestamp(maybe_invalid_datestamp()) -> boolean().
is_valid_datestamp(Date = {_Y, _M, _D}) -> calendar:valid_date(Date);
is_valid_datestamp({Date = {_, _, _}, Time = {_H, _Min, _S}}) ->
    is_valid_datestamp(Date) and is_valid_time(Time);
is_valid_datestamp(_) ->
    throw(invalid_date_format).

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Checks if given timestamp is valid.
%%% @end
%%%-------------------------------------------------------------------
-spec is_valid_time({non_neg_integer(), non_neg_integer(), non_neg_integer()}) -> boolean().
is_valid_time({H, M, S}) ->
    (0 =< H) and (H < 24) and
    (0 =< M) and (M < 60) and
    (0 =< S) and (S < 60) .

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Checks if there aren't any illegal arguments for given OAI-PMH request
%%% handled by Module.
%%% Throws if such argument exists.
%%% @end
%%%-------------------------------------------------------------------
-spec illegal_arguments_do_not_exist(oai_verb_module(), [proplists:property()]) -> ok.
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

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Checks if all arguments' values are not empty.
%%% i.e in query string "k1=v1&k2=v2&k3" k3 is empty
%%% Throws if value is empty.
%%% @end
%%%-------------------------------------------------------------------
-spec args_values_are_not_empty([proplists:property()]) -> ok.
args_values_are_not_empty(Args) ->
    lists:foreach(fun({K, V}) ->
        case V of
            true -> throw({value_is_empty, K});
            _ -> ok
        end
    end, Args).


%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Checks if key exists and is not duplicated.
%%% Throws otherwise.
%%% @end
%%%-------------------------------------------------------------------
-spec key_occurs_exactly_once(binary(), [proplists:property()]) -> true.
key_occurs_exactly_once(Key, Proplist) ->
    case count_key_occurrences(Key, Proplist) of
        0 -> throw({missing_key, Key});
        1 -> true;
        _ -> throw({repeated_key, Key})
    end.


%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Counts occurrences of a Key in Proplist.
%%% @end
%%%-------------------------------------------------------------------
-spec count_key_occurrences(binary(), [proplists:property()]) -> non_neg_integer().
count_key_occurrences(Key, Proplist) ->
    lists:foldl(fun({K, _V}, Sum) ->
        case K of
            Key -> 1 + Sum;
            _ -> Sum
        end
    end, 0, Proplist).



