%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2014, ACK CYFRONET AGH
%%% @doc
%%% Assertion macros used in ct tests
%%% @end
%%% Created : 04. May 2014 9:09 PM
%%%-------------------------------------------------------------------
-author("Tomasz Lichon").

-undef(assert).
-define(assert(BoolExpr),
	((fun () ->
		case (BoolExpr) of
			true -> ok;
			__V ->
				__Args = [{module, ?MODULE},
					{line, ?LINE},
					{expression, (??BoolExpr)},
					{expected, true},
					{value, case __V of false -> __V;
						        _ -> {not_a_boolean,__V}
					        end}],
				ct:print("assertion_failed: ~p~n", [__Args]),
				erlang:error({assertion_failed, __Args})
		end
	end)())).

-undef(assertMatch).
-define(assertMatch(Guard, Expr),
	((fun () ->
		case (Expr) of
			Guard -> ok;
			__V ->
				__Args = [{module, ?MODULE},
					{line, ?LINE},
					{expression, (??Expr)},
					{expected, (??Guard)},
					{value, __V}],
				ct:print("assertMatch_failed: ~p~n", [__Args]),
				erlang:error({assertMatch_failed, __Args})
		end
	end)())).

-undef(assertEqual).
-define(assertEqual(Expect, Expr),
	((fun (__X) ->
		case (Expr) of
			__X -> ok;
			__V ->
				__Args = [{module, ?MODULE},
					{line, ?LINE},
					{expression, (??Expr)},
					{expected, __X},
					{value, __V}],
				ct:print("assertEqual_failed: ~p~n", [__Args]),
				erlang:error({assertEqual_failed, __Args})
		end
	end)(Expect))).