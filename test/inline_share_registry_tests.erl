%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2026 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Eunit tests of the inline_share_registry module.
%%% @end
%%%-------------------------------------------------------------------
-module(inline_share_registry_tests).
-author("Lukasz Opiola").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/test/test_utils.hrl").


%%%===================================================================
%%% Tests
%%%===================================================================


empty_test() ->
    Reg = inline_share_registry:empty(),
    ?assertEqual(0, inline_share_registry:get_share_count(Reg)),
    ?assert(inline_share_registry:is_active(Reg)).


post_upgrade_test() ->
    Reg0 = inline_share_registry:post_upgrade_from_25_0(),
    ?assertNot(inline_share_registry:is_active(Reg0)),
    Reg1 = inline_share_registry:initialize_with(Reg0, generate_links(3)),
    ?assertEqual(3, inline_share_registry:get_share_count(Reg1)),
    ?assert(inline_share_registry:is_active(Reg1)).


should_utilize_test() ->
    Max = oz_worker:get_env(max_inline_share_registry_size, 1000),
    ?assert(inline_share_registry:can_fit(0)),
    ?assert(inline_share_registry:can_fit(Max - 10)),
    ?assert(inline_share_registry:can_fit(Max)),
    ?assertNot(inline_share_registry:can_fit(Max + 1)),
    ?assertNot(inline_share_registry:can_fit(Max + 1000000)).


is_active_edge_cases_test() ->
    % empty initialized -> utilized
    Reg0 = inline_share_registry:empty(),
    ?assert(inline_share_registry:is_active(Reg0)),

    % no entries but count > 0 -> NOT utilized (links held in the DB)
    Max = oz_worker:get_env(max_inline_share_registry_size, 1000),
    RegExternal = inline_share_registry:mark_migrated_to_db_links(Reg0, Max + 3),
    ?assertNot(inline_share_registry:is_active(RegExternal)),

    % attempt to add/delete links when the registry is inactive
    ?assertError({bad_state, inactive}, inline_share_registry:add_link(RegExternal, <<"a">>, <<"A">>)),
    ?assertError({bad_state, inactive}, inline_share_registry:delete_link(RegExternal, <<"a">>)),


    % requires reorganization -> not utilized
    Reg1 = inline_share_registry:post_upgrade_from_25_0(),
    ?assertNot(inline_share_registry:is_active(Reg1)),

    % attempt to add/delete links without initialization -> error
    ?assertError({bad_state, requires_reorganization}, inline_share_registry:add_link(Reg1, <<"a">>, <<"A">>)),
    ?assertError({bad_state, requires_reorganization}, inline_share_registry:delete_link(Reg1, <<"a">>)),

    % properly initialized -> utilized
    Reg2 = inline_share_registry:initialize_with(Reg1, generate_links(10)),
    ?assertEqual(10, inline_share_registry:get_share_count(Reg2)),
    ?assert(inline_share_registry:is_active(Reg2)),

    % more entries added -> utilized
    {LinkKey, LinkValue} = lists:last(generate_links(11)),
    Reg3 = inline_share_registry:add_link(Reg2, LinkKey, LinkValue),
    ?assertEqual(11, inline_share_registry:get_share_count(Reg3)),
    ?assert(inline_share_registry:is_active(Reg3)),

    % entries deleted -> utilized
    Reg4 = lists:foldl(fun(LinkKeyToDelete, AccReg) ->
        inline_share_registry:delete_link(AccReg, LinkKeyToDelete)
    end, Reg3, proplists:get_keys(generate_links(5))),
    ?assertEqual(6, inline_share_registry:get_share_count(Reg4)),
    ?assert(inline_share_registry:is_active(Reg4)).


adjust_share_count_test() ->
    Reg0 = inline_share_registry:empty(),
    Reg1 = inline_share_registry:adjust_share_count(Reg0, 5),
    ?assertEqual(5, inline_share_registry:get_share_count(Reg1)),
    Reg2 = inline_share_registry:adjust_share_count(Reg1, 1),
    ?assertEqual(6, inline_share_registry:get_share_count(Reg2)),
    Reg3 = inline_share_registry:adjust_share_count(Reg2, -5),
    ?assertEqual(1, inline_share_registry:get_share_count(Reg3)),
    Reg4 = inline_share_registry:adjust_share_count(Reg3, -1),
    ?assertEqual(0, inline_share_registry:get_share_count(Reg4)).


initialize_with_test() ->
    Max = oz_worker:get_env(max_inline_share_registry_size, 1000),
    Reg = make_registry(Max div 2),
    ?assertEqual(Max div 2, inline_share_registry:get_share_count(Reg)),
    ?assert(inline_share_registry:is_active(Reg)).


add_link_test() ->
    Reg0 = inline_share_registry:empty(),
    Reg1 = inline_share_registry:add_link(Reg0, <<"a">>, <<"A">>),
    Reg2 = inline_share_registry:add_link(Reg1, <<"b">>, <<"B">>),
    ?assertEqual(2, inline_share_registry:get_share_count(Reg2)).


delete_link_test() ->
    Reg0 = make_registry([{<<"a">>, <<"A">>}, {<<"b">>, <<"B">>}]),
    Reg1 = inline_share_registry:delete_link(Reg0, <<"a">>),
    ?assertEqual(1, inline_share_registry:get_share_count(Reg1)).


mark_migrated_to_db_links_test() ->
    Reg0 = make_registry(1000),
    CountBefore = inline_share_registry:get_share_count(Reg0),
    Reg1 = inline_share_registry:mark_migrated_to_db_links(Reg0, CountBefore),
    ?assertEqual(CountBefore, inline_share_registry:get_share_count(Reg1)),
    ?assertNot(inline_share_registry:is_active(Reg1)).


list_links_basic_test() ->
    Reg = make_registry([{<<"a">>, <<"A">>}, {<<"b">>, <<"B">>}]),
    ?assertEqual(
        [{<<"a">>, <<"A">>}, {<<"b">>, <<"B">>}],
        list(Reg, #{limit => 2})
    ).


list_links_sorted_order_test() ->
    Reg = make_registry([{<<"b">>, <<"B">>}, {<<"c">>, <<"C">>}, {<<"d">>, <<"D">>}, {<<"a">>, <<"A">>}]),
    ?assertEqual(
        [{<<"a">>, <<"A">>}, {<<"b">>, <<"B">>}, {<<"c">>, <<"C">>}, {<<"d">>, <<"D">>}],
        list(Reg, #{limit => 10})
    ).


list_links_with_offset_test() ->
    Reg = make_registry(?SHUFFLED([{<<"b">>, <<"B">>}, {<<"c">>, <<"C">>}, {<<"d">>, <<"D">>}, {<<"a">>, <<"A">>}])),
    ?assertEqual(
        [{<<"b">>, <<"B">>}, {<<"c">>, <<"C">>}, {<<"d">>, <<"D">>}],
        list(Reg, #{limit => 10, offset => 1})
    ),
    ?assertEqual(
        [{<<"c">>, <<"C">>}, {<<"d">>, <<"D">>}],
        list(Reg, #{limit => 10, offset => 2})
    ),
    ?assertEqual(
        [{<<"d">>, <<"D">>}],
        list(Reg, #{limit => 10, offset => 3})
    ),
    ?assertEqual(
        [],
        list(Reg, #{limit => 10, offset => 4})
    ),
    ?assertEqual(
        [],
        list(Reg, #{limit => 10, offset => 2342})
    ),
    ?assertEqual(
        [{<<"a">>, <<"A">>}],
        list(Reg, #{limit => 1, offset => 0})
    ),
    ?assertEqual(
        [{<<"a">>, <<"A">>}, {<<"b">>, <<"B">>}],
        list(Reg, #{limit => 2, offset => -1})
    ),
    ?assertEqual(
        [{<<"a">>, <<"A">>}, {<<"b">>, <<"B">>}, {<<"c">>, <<"C">>}, {<<"d">>, <<"D">>}],
        list(Reg, #{limit => 10, offset => -9})
    ),
    ?assertEqual(
        [{<<"a">>, <<"A">>}, {<<"b">>, <<"B">>}, {<<"c">>, <<"C">>}],
        list(Reg, #{limit => 3, offset => -1})
    ),
    ?assertEqual(
        [{<<"a">>, <<"A">>}, {<<"b">>, <<"B">>}],
        list(Reg, #{limit => 2, offset => -1000})
    ).


list_links_with_start_index_test() ->
    Reg = make_registry(?SHUFFLED([{<<"b">>, <<"B">>}, {<<"c">>, <<"C">>}, {<<"d">>, <<"D">>}, {<<"a">>, <<"A">>}])),
    ?assertEqual(
        [{<<"b">>, <<"B">>}, {<<"c">>, <<"C">>}, {<<"d">>, <<"D">>}],
        list(Reg, #{
            limit => 10,
            start_index => <<"b">>
        })
    ),
    ?assertEqual(
        [{<<"b">>, <<"B">>}],
        list(Reg, #{
            limit => 1,
            start_index => <<"b">>,
            inclusive => true
        })
    ),
    ?assertEqual(
        [{<<"c">>, <<"C">>}, {<<"d">>, <<"D">>}],
        list(Reg, #{
            limit => 2,
            start_index => <<"b">>,
            inclusive => false
        })
    ),
    ?assertEqual(
        [{<<"d">>, <<"D">>}],
        list(Reg, #{
            limit => 2,
            start_index => <<"b">>,
            inclusive => false,
            offset => 1
        })
    ),
    ?assertEqual(
        [],
        list(Reg, #{
            limit => 2,
            start_index => <<"b">>,
            inclusive => false,
            offset => 69
        })
    ),
    ?assertEqual(
        [{<<"a">>, <<"A">>}, {<<"b">>, <<"B">>}],
        list(Reg, #{
            limit => 2,
            start_index => <<"b">>,
            inclusive => false,
            offset => -2
        })
    ),
    ?assertEqual(
        [{<<"a">>, <<"A">>}, {<<"b">>, <<"B">>}],
        list(Reg, #{
            limit => 2,
            start_index => <<"b">>,
            inclusive => false,
            offset => -3445
        })
    ).


list_links_with_nonpresent_start_index_test() ->
    Reg = make_registry(?SHUFFLED([{<<"c">>, <<"C">>}, {<<"d">>, <<"D">>}, {<<"a">>, <<"A">>}])),
    ?assertEqual(
        [{<<"c">>, <<"C">>}, {<<"d">>, <<"D">>}],
        list(Reg, #{
            limit => 10,
            start_index => <<"b">>
        })
    ),
    ?assertEqual(
        [{<<"c">>, <<"C">>}],
        list(Reg, #{
            limit => 1,
            start_index => <<"b">>,
            inclusive => true
        })
    ),
    ?assertEqual(
        [{<<"c">>, <<"C">>}, {<<"d">>, <<"D">>}],
        list(Reg, #{
            limit => 2,
            start_index => <<"b">>,
            inclusive => false
        })
    ),
    ?assertEqual(
        [{<<"d">>,<<"D">>}],
        list(Reg, #{
            limit => 2,
            start_index => <<"b">>,
            inclusive => false,
            offset => 1
        })
    ),
    ?assertEqual(
        [],
        list(Reg, #{
            limit => 2,
            start_index => <<"b">>,
            inclusive => false,
            offset => 2
        })
    ),
    ?assertEqual(
        [],
        list(Reg, #{
            limit => 2,
            start_index => <<"b">>,
            inclusive => false,
            offset => 69
        })
    ),
    ?assertEqual(
        [{<<"a">>, <<"A">>}, {<<"c">>, <<"C">>}],
        list(Reg, #{
            limit => 2,
            start_index => <<"b">>,
            inclusive => false,
            offset => -2
        })
    ),
    ?assertEqual(
        [{<<"a">>, <<"A">>}, {<<"c">>, <<"C">>}],
        list(Reg, #{
            limit => 2,
            start_index => <<"b">>,
            inclusive => false,
            offset => -20
        })
    ).


%% NOTE: listing tests on bigger collections are included in @see share_registry_test_SUITE


encode_decode_test() ->
    lists:foreach(fun(Record) ->
        ?assert(eunit_utils:is_equal_after_db_encode_and_decode(Record)),
        ?assert(eunit_utils:is_equal_after_json_encode_and_decode(Record))
    end, [
        inline_share_registry:empty(),
        inline_share_registry:post_upgrade_from_25_0(),
        make_registry(1),
        make_registry(999),
        inline_share_registry:mark_migrated_to_db_links(make_registry(50), 50),
        inline_share_registry:adjust_share_count(make_registry(50), 6),
        inline_share_registry:adjust_share_count(make_registry(14), -14)
    ]).


%%%===================================================================
%%% Helpers
%%%===================================================================


make_registry(Count) when is_integer(Count) ->
    make_registry(generate_links(Count));

make_registry(Links) ->
    inline_share_registry:initialize_with(
        inline_share_registry:empty(),
        Links
    ).


generate_links(Count) ->
    lists_utils:generate(fun(Ordinal) ->
        {<<"key", (integer_to_binary(Ordinal))/binary>>, <<"value", (integer_to_binary(Ordinal))/binary>>}
    end, Count).


list(Registry, ListingOpts) ->
    DbLinks = inline_share_registry:list_links(Registry, ListingOpts),
    db_links_to_proplist(DbLinks).


db_links_to_proplist(Links) ->
    [{L#link.name, L#link.target} || L <- Links].


-endif.

