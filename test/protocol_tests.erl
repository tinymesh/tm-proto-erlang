-module(protocol_tests).

-include_lib("eunit/include/eunit.hrl").

-define(sysid, 1).

unserialize_test() ->
	{ok, Buf} = file:read_file("../test/data/messages.b64"),
	[ ?assertMatch(
		{ok, _, <<>>}, tinymesh:unserialize( base64:decode(binary_to_list(X))))
			|| <<_:88, X/binary>> <- binary:split(Buf,  <<10>>, [global])].

cmd_twoway_test() ->
	Cmd = fun(C, ID, N, Pre) ->
		[{<<"type">>, <<"command">>}, {<<"uid">>, ID},
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
		{ok, [M2], <<>>}  = tinymesh:unserialize(Buf),

		?assertEqual(
			lists:usort(Item),
			lists:usort(M2)),

		{[Buf | AccB], [M2 | AccI]}
	end, {[], []}, Cmds),

	%% Check that we can do all at once
	{ok, Res, <<>>} = tinymesh:unserialize(iolist_to_binary(Buf)),
	?assertEqual(Items, Res),
	?assertEqual(length(Cmds), length(Res)).

-define(pd(K, S), get(<<((atom_to_binary(K, utf8)))/binary, $:, S:16/integer>>)).
-define(pd(K, S, V), put(<<((atom_to_binary(K, utf8)))/binary, $:, S:16/integer>>, V)).
-define(pdi(K, S)
	, put(<<((atom_to_binary(K, utf8)))/binary, $:, S:16/integer>>, ?pd(K, S) + 1)).

general_event_toway_test() ->
	Nodes = lists:map(fun(N) ->
		?pd(networklvl, N, random:uniform(10)),
		?pd(jump_count, N, random:uniform(10)),
		?pd(packet_num, N, random:uniform(16#FFFF)),
		N
	end, lists:seq(0, 100)),

	[lists:foreach(fun(Y) ->
		%% Some nodes likes serial more than others
		M0 = lists:ukeysort(1, create_msg(N + Y band 32 bsr 7, N, Y)),
		{ok, [M1]} = tinymesh:serialize(M0),
		{ok, [M2], <<>>} = tinymesh:unserialize(M1),
		?assertEqual(M0, lists:ukeysort(1, M2))
	end, lists:seq(0, 500)) || N <- Nodes].


msg_base(ID, Rest) ->
	[ {<<"sid">>, ?sysid}, {<<"uid">>, ID}
	, {<<"rssi">>, random:uniform(255)}, {<<"network_lvl">>, ?pd(networklvl, ID)}
	, {<<"jump_count">>, ?pd(jump_count, ID)}, {<<"latency">>, random:uniform(512)}
	, {<<"packet_num">>, ?pdi(packet_num, ID)} | Rest ].

%% event:serial
create_msg(1, N, Y) ->
	Buf = <<"test data for ", N:8/integer, ":", Y:16/integer>>,
	msg_base(N, [{<<"type">>, <<"event">>}, {<<"detail">>,
<<"serial">>}, {<<"sequence">>, 0}, {<<"serial">>, Buf}]);

%% event:get_config
create_msg(0, N, _Y) ->
	Ev =
		[ {<<"type">>, <<"event">>}
		, {<<"detail">>, <<"ima">>}
		, {<<"msg_data">>, 0}
		, {<<"locator">>, 0}
		, {<<"temp">>, 22}
		, {<<"voltage">>, 3.33}
		, {<<"analog_io_0">>, 0}
		, {<<"analog_io_1">>, 0}
		, {<<"hardware">>, <<"2.00">>}
		, {<<"firmware">>, <<"1.23">>}
		| [ {<<"digital_io_", ((X + 48))>>, random:uniform(1)} || X <- lists:seq(0, 7) ]
		],
	msg_base(N, Ev).

% @todo 2013-05-28; fix event:get_path / event:get_config generation
%create_msg(2, N) ->
%create_msg(3, N) ->


partial_test() ->
	{A, B} = {<<35,1,0,0,0,4,1,0,0,92,1,1,161,173>>,
	          <<0,0,2,14,0:32,0,0,121,187,0,0:16,0:16,2,0,1,22>>},
	{ok, _, Rest} = tinymesh:unserialize(<<B/binary, A/binary>>, A),
	?assertMatch({ok, _, <<>>}, tinymesh:unserialize(B, Rest)),

	Part2 = <<35,1,0,0,0,4,2,0,0,185,4,4,28,77,0,0,2,14,1,197,1,102,0,87,156>>,
	Partial = <<
	  35,1,0,0,0,1,2,0,0,157,2,2,28,46,0,0,2,14,1,203,1,96,0,105,167,111,220,7,255,0,1,2,0,1,34
	, Part2/binary>>,

	?assertMatch({ok, [_], Part2}, tinymesh:unserialize(Partial)),

	P1 = <<
	  35,1,0,0,0,1,2,0,0,157,2,2,28,46,0,0,2,14,1,203,1,96,0,105,167,111,220,7,255,0,1,2,0,1,34
	, 35,1,0,0,0,4,2,0,0,185,4,4,28,77,0,0,2,14,1,197,1,102,0,87,156>>,
	P2 = <<
	  113,252,2,61,0,12,2,0,1,34
	, 35,1,0,0,0,0,1,0,0,0,0,0,101,29,0,0,2,14,0,0,0,0,0,0,121,187,0,0,0,0,0,2,0,1,34>>,

	{ok, [_], Buf} = tinymesh:unserialize(P1),
	?assertMatch({ok, [_,_], <<>>}, tinymesh:unserialize(P2, Buf)).

frame_long_serial_test() ->
	NumItems = 10,
	CmdNum = 10,
	Data = binary:copy(<<"a">>, 120 * NumItems),
	Fun = fun(N, D)->
		[ {<<"type">>, <<"command">>}, {<<"uid">>, 123}
		, {<<"cmd_number">>, N}, {<<"command">>, <<"serial">>}
		, {<<"data">>, D} ] end,

	{ok, Items} = tinymesh:serialize(Fun(CmdNum, Data)),

	Num = lists:foldl(fun(Item, I) ->
		Match0 = lists:ukeysort(1, Fun(I, binary:copy(<<"a">>, 120))),
		{ok, [Match1], <<>>} = tinymesh:unserialize(Item),
		?assertEqual(Match0, lists:ukeysort(1, Match1)),
		I + 1
	end, CmdNum, Items),
	?assertEqual(NumItems, Num - CmdNum),

	%% Check that CmdNum is bounded
	{ok, [_,_] = Serialized} = tinymesh:serialize(Fun(255, binary:copy(<<"b">>, 120 * 2))),
	{ok, [M1, M2], <<>>} = tinymesh:unserialize(iolist_to_binary(Serialized)),
	?assertEqual({<<"cmd_number">>, 255}, lists:keyfind(<<"cmd_number">>, 1, M1)),
	?assertEqual({<<"cmd_number">>,   1}, lists:keyfind(<<"cmd_number">>, 1, M2)).


unserialize_config_test() ->
	Msg = fun(Config) ->
		[ {<<"type">>, <<"command">>}, {<<"uid">>, 16#44332211}
		, {<<"cmd_number">>, 201}, {<<"command">>, <<"set_config">>}
		, {<<"config">>, Config} ] end,

	{ok, [<<Buf0:8/binary, _:8, Buf1/binary>> = Buf]} =
		tinymesh:serialize(Msg([{<<"rf_power">>, 10}])),

	Res = tinymesh:unserialize(<<Buf0/binary, 255/integer, Buf1/binary>>),
	?assertEqual({error, {invalid_config_index, 255}}, Res),

	      Match0   = lists:ukeysort(1, Msg([{<<"rf_power">>, 10}])),
	{ok, [Match1], <<>>} = tinymesh:unserialize(Buf),
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
		[ {<<"type">>, <<"command">>}, {<<"uid">>, 16#11223344}
		, {<<"cmd_number">>, 200}, {<<"command">>, <<"set_config">>}
		, {<<"config">>, Config} ],

	?assertMatch({ok, [_,_]}, tinymesh:serialize(Msg)).
