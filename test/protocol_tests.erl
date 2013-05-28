-module(protocol_tests).

-include_lib("eunit/include/eunit.hrl").

unserialize_test() ->
	{ok, Buf} = file:read_file("../test/data/messages.b64"),
	[ ?assertMatch(
		{ok, _}, tinymesh:unserialize( base64:decode(binary_to_list(X))))
			|| <<_:88, X/binary>> <- binary:split(Buf,  <<10>>, [global])].

twoway_test() ->
	Cmd = fun(C, ID, N, Pre) ->
		[{<<"type">>, <<"command">>}, {<<"unique_id">>, ID},
		 {<<"cmd_number">>, N}, {<<"command">>, C} | Pre]
	end,

	Cmds = [
	  Cmd(<<"set_output">>, 1, 2, [{<<"output">>, [{1,0}, {3,1}]}])
	, Cmd(<<"set_pwm">>, 2, 3, [{<<"pwm">>, 123}])
	, Cmd(<<"init_gw_config">>, 3, 4, [])
	, Cmd(<<"get_cid">>, 4, 5, [])
	, Cmd(<<"get_status">>, 5, 6, [])
	, Cmd(<<"get_did_status">>, 6, 7, [])
	, Cmd(<<"get_config">>, 7, 8, [])
	, Cmd(<<"force_reset">>, 8, 9, [])
	, Cmd(<<"get_path">>, 9, 10, [])
	],

	{Buf, Items} = lists:foldl(fun(Item, {AccB, AccI}) ->
		{ok, [Buf]} = tinymesh:serialize(Item),
		{ok, [M2]}  = tinymesh:unserialize(Buf),

		?assertEqual(
			lists:usort(Item),
			lists:usort(M2)),

		{[Buf | AccB], [M2 | AccI]}
	end, {[], []}, Cmds),

	%% Check that we can do all at once
	{ok, Res} = tinymesh:unserialize(iolist_to_binary(Buf)),
	?assertEqual(Items, Res),
	?assertEqual(length(Cmds), length(Res)).


partial_test() ->
	{A, B} = {<<35,1,0,0,0,4,1,0,0,92,1,1,161,173>>,
	          <<0,0,2,14,0:32,0,0,121,187,0,0:16,0:16,2,0,1,22>>},
	{ok, _, Rest} = tinymesh:unserialize(<<B/binary, A/binary>>, A),
	?assertMatch({ok, _}, tinymesh:unserialize(B, Rest)).


frame_long_serial_test() ->
	NumItems = 10,
	CmdNum = 10,
	Data = binary:copy(<<"a">>, 120 * NumItems),
	Fun = fun(N, D)->
		[ {<<"type">>, <<"command">>}, {<<"unique_id">>, 123}
		, {<<"cmd_number">>, N}, {<<"command">>, <<"serial">>}
		, {<<"data">>, D} ] end,

	{ok, Items} = tinymesh:serialize(Fun(CmdNum, Data)),

	Num = lists:foldl(fun(Item, I) ->
		Match0 = lists:ukeysort(1, Fun(I, binary:copy(<<"a">>, 120))),
		{ok, [Match1]} = tinymesh:unserialize(Item),
		?assertEqual(Match0, lists:ukeysort(1, Match1)),
		I + 1
	end, CmdNum, Items),

	?assertEqual(NumItems, Num - CmdNum).
