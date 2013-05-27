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
	?assertEqual(Items, lists:reverse(Res)),
	?assertEqual(length(Cmds), length(Res)).
