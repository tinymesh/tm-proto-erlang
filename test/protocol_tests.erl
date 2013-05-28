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
	?assertEqual(NumItems, Num - CmdNum),

	%% Check that CmdNum is bounded
	{ok, [_,_] = Serialized} = tinymesh:serialize(Fun(255, binary:copy(<<"b">>, 120 * 2))),
	{ok, [M1, M2]} = tinymesh:unserialize(iolist_to_binary(Serialized)),
	?assertEqual({<<"cmd_number">>, 255}, lists:keyfind(<<"cmd_number">>, 1, M1)),
	?assertEqual({<<"cmd_number">>,   1}, lists:keyfind(<<"cmd_number">>, 1, M2)).


unserialize_config_test() ->
	Msg = fun(Config) ->
		[ {<<"type">>, <<"command">>}, {<<"unique_id">>, 16#44332211}
		, {<<"cmd_number">>, 201}, {<<"command">>, <<"set_config">>}
		, {<<"config">>, Config} ] end,

	{ok, [<<Buf0:8/binary, _:8, Buf1/binary>> = Buf]} =
		tinymesh:serialize(Msg([{<<"rf_power">>, 10}])),

	Res = tinymesh:unserialize(<<Buf0/binary, 255/integer, Buf1/binary>>),
	?assertEqual({error, {invalid_config_index, 255}}, Res),

	      Match0   = lists:ukeysort(1, Msg([{<<"rf_power">>, 10}])),
	{ok, [Match1]} = tinymesh:unserialize(Buf),
	io:format("mjau: ~p~n", [Match1]),
	?assertEqual(Match0, lists:ukeysort(1, Match1)).

%	?assertEqual(
%		  {error, {invalid_config_index, unknown_key}}
%		, tinymesh:serialize(Msg([{unknown_key, -1}]))).

%% command:set_config messages can be dropped if they exceed > 20 items
split_config_test() ->
	Config =
		[ {rf_channel, 1}, {rf_power, 1}, {rf_data_rate, 1}
		, {protocol_mode, 1}, {rssi_threshold, 1}, {rssi_assesment, 1}
		, {hiam_time, 1}, {ima_time, 1}, {connect_check_time, 1}
		, {max_jump_level, 1}, {max_jump_count, 1}, {max_packet_latency, 1}
		, {rf_retry_limit, 1}, {serial_timeout, 1}, {device_type, 1}
		, {gpio_0_config, 1}, {gpio_1_config, 1}, {gpio_2_config, 1}
		, {gpio_3_config, 1}, {gpio_4_config, 1}, {gpio_5_config, 1}
		, {gpio_6_config, 1}, {gpio_7_config, 1}, {gpio_0_trigger, 1}
		, {gpio_1_trigger, 1}, {gpio_2_trigger, 1}, {gpio_3_trigger, 1}
		, {gpio_4_trigger, 1}, {gpio_5_trigger, 1}, {gpio_6_trigger, 1}
		, {gpio_7_trigger, 1}],

	Msg =
		[ {<<"type">>, <<"command">>}, {<<"unique_id">>, 16#11223344}
		, {<<"cmd_number">>, 200}, {<<"command">>, <<"set_config">>}
		, {<<"config">>, Config} ],

	?assertMatch({ok, [_,_]}, tinymesh:serialize(Msg)).
