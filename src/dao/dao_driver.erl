%% ===================================================================
%% @author Tomasz Lichon
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module provides application specific functions needed by dao
%% library to work with database
%% @end
%% ===================================================================
-module(dao_driver).
-author("Tomasz Lichon").

-include_lib("dao/dao_driver.hrl").

%% API
-export([set_db/1, get_db/0, record_info/1, is_valid_record/1]).
-export([doc_to_term/1, term_to_doc/1]).

%% set_db/1
%% ====================================================================
%% @doc Sets current working database name
%% @end
-spec set_db(DbName :: string()) -> ok.
%% ====================================================================
set_db(DbName) ->
    put(current_db, DbName).

%% get_db/0
%% ====================================================================
%% @doc Gets current working database name
%% @end
-spec get_db() -> DbName :: string().
%% ====================================================================
get_db() ->
    case get(current_db) of
        DbName when is_list(DbName) ->
            DbName;
        _ ->
            ?DEFAULT_DB
    end.

%% is_valid_record/1
%% ====================================================================
%% @doc Checks if given record/record name is supported and existing record
%% @end
-spec is_valid_record(Record :: atom() | string() | tuple()) -> boolean().
%% ====================================================================
is_valid_record(Record) when is_list(Record) ->
    is_valid_record(list_to_atom(Record));
is_valid_record(Record) when is_atom(Record) ->
    case ?dao_record_info(Record) of
        {_Size, _Fields, _} -> true;    %% When checking only name of record, we omit size check
        _ -> false
    end;
is_valid_record(Record) when not is_tuple(Record); not is_atom(element(1, Record)) ->
    false;
is_valid_record(Record) ->
    case ?dao_record_info(element(1, Record)) of
        {Size, Fields, _} when is_list(Fields), tuple_size(Record) =:= Size ->
            true;
        _ -> false
    end.

%% record_info/1
%% ====================================================================
%% @doc Returns info about given record
%% @end
-spec record_info(Record :: atom() | string() | tuple()) -> boolean().
%% ====================================================================
record_info(Record) ->
    ?dao_record_info(Record).

%% term_to_doc/1
%% ====================================================================
%% @doc Converts given erlang term() into valid BigCouch document body. Given term should be a record. <br/>
%% All erlang data types are allowed, although using binary() is not recommended (because JSON will treat it like a string and will fail to read it)
%% @end
-spec term_to_doc(Field :: term()) -> term().
%% ====================================================================
term_to_doc(Field) when is_number(Field) ->
    Field;
term_to_doc(Field) when is_boolean(Field); Field =:= null ->
    Field;
term_to_doc(Field) when is_pid(Field) ->
    list_to_binary(?RECORD_FIELD_PID_PREFIX ++ pid_to_list(Field));
term_to_doc(Field) when is_binary(Field) ->
    <<<<?RECORD_FIELD_BINARY_PREFIX>>/binary, Field/binary>>;   %% Binary is saved as string, so we add a prefix
term_to_doc(Field) when is_list(Field) ->
    case io_lib:printable_unicode_list(Field) of
        true -> dao_helper:name(Field);
        false -> [term_to_doc(X) || X <- Field]
    end;
term_to_doc(Field) when is_atom(Field) ->
    term_to_doc(?RECORD_FIELD_ATOM_PREFIX ++ atom_to_list(Field));  %% Atom is saved as string, so we add a prefix
term_to_doc(Field) when is_tuple(Field) ->
    IsRec = is_valid_record(Field),

    {InitObj, LField, RecName} =  %% Prepare initial structure for record or simple tuple
    case IsRec of
        true ->
            [RecName1 | Res] = tuple_to_list(Field),
            {dao_json:mk_field(dao_json:mk_obj(), ?RECORD_META_FIELD_NAME, dao_json:mk_str(atom_to_list(RecName1))), Res, RecName1};
        false ->
            {dao_json:mk_obj(), tuple_to_list(Field), none}
    end,
    FoldFun = fun(Elem, {Poz, AccIn}) ->  %% Function used in lists:foldl/3. It parses given record/tuple field
        case IsRec of                 %% and adds to Accumulator object
            true ->
                {_, Fields, _} = ?dao_record_info(RecName),

                Value = term_to_doc(Elem),

                {Poz + 1, dao_json:mk_field(AccIn, atom_to_list(lists:nth(Poz, Fields)), Value)};
            false ->
                {Poz + 1, dao_json:mk_field(AccIn, ?RECORD_TUPLE_FIELD_NAME_PREFIX ++ integer_to_list(Poz), term_to_doc(Elem))}
        end
    end,
    {_, {Ret}} = lists:foldl(FoldFun, {1, InitObj}, LField),
    {lists:reverse(Ret)};
term_to_doc(Field) ->
    lager:error("Cannot convert term to document because field: ~p is not supported", [Field]),
    throw({unsupported_field, Field}).


%% doc_to_term/1
%% ====================================================================
%% @doc Converts given valid BigCouch document body into erlang term().
%% If document contains saved record which is a valid record (see is_valid_record/1),
%% then structure of the returned record will be updated
%% @end
-spec doc_to_term(Field :: term()) -> term().
%% ====================================================================
doc_to_term(Field) when is_number(Field); is_atom(Field) ->
    Field;
doc_to_term(Field) when is_binary(Field) -> %% Binary type means that it is atom, string or binary.
    SField = binary_to_list(Field),         %% Prefix tells us which type is it
    BinPref = string:str(SField, ?RECORD_FIELD_BINARY_PREFIX),
    AtomPref = string:str(SField, ?RECORD_FIELD_ATOM_PREFIX),
    PidPref = string:str(SField, ?RECORD_FIELD_PID_PREFIX),
    if
        BinPref == 1 -> list_to_binary(string:sub_string(SField, length(?RECORD_FIELD_BINARY_PREFIX) + 1));
        AtomPref == 1 -> list_to_atom(string:sub_string(SField, length(?RECORD_FIELD_ATOM_PREFIX) + 1));
        PidPref == 1 ->
            PidString = string:sub_string(SField, length(?RECORD_FIELD_PID_PREFIX) + 1),
            try list_to_pid(PidString) of %(temporary fix) todo change our pid storing mechanisms, so such conversion won't fail
                Pid -> Pid
            catch
                _:_Error ->
                    lager:warning("Cannot convert document to term: cannot read PID ~p. Node missing?", [PidString]),
                    undefined
            end;
        true -> unicode:characters_to_list(list_to_binary(SField))
    end;
doc_to_term(Field) when is_list(Field) ->
    [doc_to_term(X) || X <- Field];
doc_to_term({Fields}) when is_list(Fields) -> %% Object stores tuple which can be an erlang record
    Fields1 = [{binary_to_list(X), Y} || {X, Y} <- Fields],
    {IsRec, FieldsInit, RecName} =
        case lists:keyfind(?RECORD_META_FIELD_NAME, 1, Fields1) of  %% Search for record meta field
            {_, RecName1} -> %% Meta field found. Check if it is valid record name. Either way - prepare initial working structures
                {case is_valid_record(binary_to_list(RecName1)) of true -> true; _ -> partial end,
                    lists:keydelete(?RECORD_META_FIELD_NAME, 1, Fields1), list_to_atom(binary_to_list(RecName1))};
            _ ->
                DataTmp = [{list_to_integer(lists:filter(fun(E) -> (E >= $0) andalso (E =< $9) end, Num)), Data} || {Num, Data} <- Fields1],
                {false, lists:sort(fun({A, _}, {B, _}) -> A < B end, DataTmp), none}
        end,
    case IsRec of
        false -> %% Object is an tuple. Simply create tuple from successive fields
            list_to_tuple([doc_to_term(Data) || {_, Data} <- FieldsInit]);
        partial -> %% Object is an unsupported record. We are gonna build record based only on current structure from DB
            list_to_tuple([RecName | [doc_to_term(Data) || {_, Data} <- FieldsInit]]);
        true -> %% Object is an supported record. We are gonna build record based on current erlang record structure (new fields will get default values)
            {_, FNames, InitRec} = ?dao_record_info(RecName),
            FoldFun = fun(Elem, {Poz, AccIn}) ->
                case lists:keyfind(atom_to_list(Elem), 1, FieldsInit) of
                    {_, Data} ->
                        {Poz + 1, setelement(Poz, AccIn, doc_to_term(Data))};
                    _ ->
                        {Poz + 1, AccIn}
                end
            end,
            {_, Ret} = lists:foldl(FoldFun, {2, InitRec}, FNames),
            Ret
    end;
doc_to_term(_) ->
    throw(invalid_document).