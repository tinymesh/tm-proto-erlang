-module(tinymesh).

-export([unserialize/1, serialize/1]).

-type buf()     :: binary().
-type stream()  :: binary().
-type parsed()  :: [{binary(), binary() | integer() | list(tuple())}].

-record(command, {
	  target  = <<0,0,0,0>>   :: binary()
	, type    = 0             :: non_neg_integer()
	, command = 0             :: non_neg_integer()
	, number  = 0             :: non_neg_integer()
	, payload = <<0,0>>       :: binary()
}).

-spec unserialize(stream() | buf()) -> {ok, [parsed()] | parsed()} | {error, term()}.
unserialize(<<Chksum:8, _/binary>> = Buf) when byte_size(Buf) == Chksum ->
	proc(Buf);
unserialize(<<Chksum:8, _/binary>> = Bin) when byte_size(Bin) =/= Chksum ->
	{error, checksum}.

-spec proc(buf()) -> {ok, parsed()}.
proc(<<Checksum:8/unsigned-integer,         % Message checksum
       SystemID:32/little-unsigned-integer, % System wide ID
       UniqueID:32/little-unsigned-integer, % Address for router
       RSSI:8/unsigned-integer,             % RSSI to first receiver
       NetworkLevel:8/unsigned-integer,     % Vertical hops to gateway
       HopCounter:8/unsigned-integer,       % Actual hop count to gateway
       Counter:16/unsigned-integer,         % Unique message number
       Latency:16/unsigned-integer,         % Time since msg creation (10ms)
       Payload/binary>> = Buf) when byte_size(Buf) == Checksum ->
	proc( Payload
		, fun ppayload/1
		, [ {<<"system_id">>,      SystemID}
	      , {<<"unique_id">>,      UniqueID}
	      , {<<"rssi">>,           RSSI}
	      , {<<"network_lvl">>,    NetworkLevel}
	      , {<<"jump_count">>,     HopCounter}
	      , {<<"msg_id">>,         Counter}
	      , {<<"latency">>,        Latency}]);
proc(_) ->
	{error, invalid_data}.

-spec proc(binary(), fun(), parsed()) -> {ok, parsed() | []} | {error, term()}.
proc(<<>>, _, Acc) ->
	{ok, Acc};
proc(Binary, Fun, Acc) ->
	case Fun(Binary) of
		{ok, _, Acc2} -> {ok, lists:ukeymerge(1, Acc2, Acc)};
%		{ok, Bin2, Acc2, Fun2} -> proc(Bin2, lists:ukeymerge(1, Acc2, Acc), Fun2);
		{error, Err} -> {error, Err}
	end.

-spec ppayload(binary()) -> {ok, binary(), parsed(), fun()} | {ok, binary(), parsed()} | {error, term()}.
ppayload(<<16:8, 0:8, Buf/binary>>) ->
	{ok, <<>>, [ {<<"type">>, <<"serial">>}
	           , {<<"serial">>, Buf} ]};
ppayload(<<2:8, Buf/binary>>) ->
	ppayload(Buf, {1, []});
ppayload(_) ->
	{error, invalid_data}.

ppayload(<<>>, {_, Acc}) -> {ok, <<>>, lists:reverse(Acc)};
ppayload(<<Detail:8, Tail/binary>>, {1 = P, Acc}) ->
	ppayload(Tail, {P+1, [{<<"detail">>, event_detail(Detail)}|Acc]});
ppayload(<<Data:16/integer-unit:1, Tail/binary>>, {2 = P, Acc}) ->
	ppayload(Tail, {P+2, [{<<"msg_data">>, Data}|Acc]});
ppayload(<<Locator:32/unsigned-integer, Tail/binary>>, {4 = P, Acc}) ->
	ppayload(Tail, {P+4, [{<<"locator">>, Locator}|Acc]});
ppayload(<<Temperatur:8/unsigned-integer, Tail/binary>>, {8 = P, Acc}) ->
	ppayload(Tail, {P+1, [{<<"temperature">>, Temperatur}|Acc]});
ppayload(<<Voltage:8/unsigned-integer, Tail/binary>>, {9 = P, Acc}) ->
	Voltage2 = erlang:trunc((Voltage*0.03)*100)*0.01,
	ppayload(Tail, {P+1, [{<<"voltage">>, Voltage2}|Acc]});
ppayload(<<D0:1, D1:1, D2:1, D3:1, D4:1, D5:1, D6:1, D7:1, Tail/binary>>, {10 = P, Acc}) ->
	ppayload(Tail, {P+1,
		[{<<"digital_io_0">>, D0}|
		 [{<<"digital_io_1">>, D1}|
		  [{<<"digital_io_2">>, D2}|
		   [{<<"digital_io_3">>, D3}|
		    [{<<"digital_io_4">>, D4}|
		     [{<<"digital_io_5">>, D5}|
		      [{<<"digital_io_6">>, D6}|
		       [{<<"digital_io_7">>, D7}|Acc]]]]]]]]});
ppayload(<<AIn0:16/unsigned-integer, Tail/binary>>, {11 = P, Acc}) ->
	ppayload(Tail, {P+2, [{<<"analog_io_0">>, AIn0}|Acc]});
ppayload(<<AIn1:16/unsigned-integer, Tail/binary>>, {13 = P, Acc}) ->
	ppayload(Tail, {P+2, [{<<"analog_io_1">>, AIn1}|Acc]});
ppayload(<<Major:8/integer, Min:8/integer, Tail/binary>>, {15 = P, Acc}) ->
	ppayload(Tail, {P+2, [{<<"hardware">>, Major + (Min / 100)}|Acc]});
ppayload(<<Major:8/integer, Min:8/integer, Tail/binary>>, {17 = P, Acc}) ->
	ppayload(Tail, {P+2, [{<<"firmware">>, Major + (Min / 100)}|Acc]}).

-spec event_detail(1..16) -> atom().
event_detail(16#01) -> io_change;
event_detail(16#02) -> analog_io_0_change;
event_detail(16#03) -> analog_io_1_change;
event_detail(16#04) -> temp_warning;
event_detail(16#05) -> voltage_warning;
event_detail(16#08) -> power_on;
event_detail(16#09) -> ima;
event_detail(16#0A) -> network_busy; %% co-existing with same system id
event_detail(16#0B) -> network_inactiv;
event_detail(16#0C) -> channel_jam; %% no cca
event_detail(16#0D) -> shared_channel;
event_detail(16#0E) -> zacima;
event_detail(16#10) -> ack;
event_detail(16#11) -> not_accetable;
event_detail(16#12) -> nid_report;
event_detail(16#21) -> get_config.

-spec serialize(parsed()) -> buf().
serialize(Payload) when is_list(Payload) ->
	serialize_proc(Payload, fun s_select_type/2, []).

-spec serialize_proc(parsed(), fun(), any()) -> {ok, binary()} | {error, term()}.
serialize_proc(Payload, Fun, Acc) ->
	case Fun(Payload, Acc) of
		{ok, Fun2, Acc2} when is_function(Fun2) ->
			serialize_proc(Payload, Fun2, Acc2);
		{ok, Acc2} -> {ok, Acc2};
		{error, Err} -> {error, Err}
	end.

s_select_type(Payload, []) ->
	keymember(<<"type">>, Payload, fun s_select_type2/2, msg_type, []).
s_select_type2({<<"type">>, <<"command">>}, []) ->
	 {ok, fun s_target/2, #command{}};
s_select_type2({<<"type">>, _}, []) ->
	 {error, msg_type}.

s_target(Payload, #command{} = Acc) ->
	keymember(<<"unique_id">>, Payload, fun s_target2/2, msg_destination, Acc).

s_target2({<<"unique_id">>, Val}, #command{} = Acc) when is_integer(Val) ->
	{ok, fun s_number/2, Acc#command{target = Val}}.

s_number(Payload, #command{} = Acc) ->
	keymember(<<"packet_number">>, Payload, fun s_number2/2, packet_number, Acc).

s_number2({<<"packet_number">>, Val}, #command{} = Acc) when is_integer(Val) ->
	{ok, fun s_command/2, Acc#command{number = Val}}.

s_command(Payload, #command{} = Acc) ->
	case lists:keyfind(<<"command">>, 1, Payload) of
		{<<"command">>, <<"serial">>} ->
			{ok, fun c_serial/2, Acc#command{type = 17}};
		{<<"command">>, <<"set_output">>} ->
			{ok, fun c_set_output/2, Acc#command{type = 3, command = 1}};
		{<<"command">>, <<"set_pwm">>} ->
			{ok, fun c_set_pwm/2, Acc#command{type = 3, command = 2}};
		{<<"command">>, <<"set_config">>} ->
			{ok, fun c_set_config/2, Acc#command{type = 3, command = 3}};
		{<<"command">>, <<"get_nid">>} ->
			{ok, fun c_serialize/2, Acc#command{type = 3, command = 16}};
		{<<"command">>, <<"get_status">>} ->
			{ok, fun c_serialize/2, Acc#command{type = 3, command = 17}};
		{<<"command">>, <<"get_config">>} ->
			{ok, fun c_serialize/2, Acc#command{type = 3, command = 19}};
		_ ->
			{error, unknown_command}
	end.

c_serial(Payload, #command{} = Acc) ->
	keymember(<<"data">>, Payload, fun c_serial2/2, no_data, Acc).

c_serial2({<<"data">>, Val}, #command{} = Acc) when is_binary(Val) ->
	{ok, fun c_serialize/2, Acc#command{payload = Val}}.

c_set_output(Payload, #command{} = Acc) ->
	keymember(<<"output">>, Payload, fun c_set_output2/2, no_output, Acc).

c_set_output2({<<"output">>, Payload}, Acc) ->
	{A1, A2} = lists:foldl(fun c_set_output3/2, {0, 0}, Payload),
	{ok, fun c_serialize/2, Acc#command{payload = <<A1/integer, A2/integer>>}}.

c_set_output3({N, true}, {A1, A2}) -> {A1 + erlang:trunc(math:pow(2, N)), A2};
c_set_output3({N, false}, {A1, A2}) -> {A1, erlang:trunc(A2 + math:pow(2, N))};
c_set_output3({_, none}, {A1, A2}) -> {A1, A2}.

c_set_pwm(Payload, #command{} = Acc) ->
	keymember(<<"pwm">>, Payload, fun c_set_pwm2/2, missing_pwm, Acc).

c_set_pwm2({<<"pwm">>, Val}, #command{} = Acc) ->
	{ok, fun c_serialize/2, Acc#command{payload = <<Val:8/integer, 0>>}}.

c_set_config(Payload, #command{} = Acc) ->
	keymember(<<"config">>, Payload, fun c_set_config2/2, missing_config, Acc).

c_set_config2({<<"config">>, Payload}, #command{} = Acc) ->
	Config  = iolist_to_binary(lists:flatten(tinymesh_config:pack(Payload))),
	{ok, fun c_serialize/2, Acc#command{payload = <<Config/binary, 0:(32-byte_size(Config))/integer-unit:8>>}}.

c_serialize(_, #command{target = D, number = N, type = T, command = 0, payload = P} = Acc) ->
	Out = <<D:32/integer-little, N:8/integer, T:8/integer, P/binary>>,
	Size = byte_size(Out) + 1,
	{ok, <<Size/integer, Out/binary>>};
c_serialize(_, #command{target = D, number = N, type = T, command = C, payload = P} = Acc) ->
	Out = <<D:32/integer-little, N:8/integer, T:8/integer, C:8/integer, P/binary>>,
	Size = byte_size(Out) + 1,
	{ok, <<Size/integer, Out/binary>>}.

keymember(K, L, Fun, E, Acc) when is_list(L) ->
	case lists:keyfind(K, 1, L) of
		{K, V} -> Fun({K, V}, Acc);
		false -> {error, E}
	end.

-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").

	-define(BASEMSG, 1,0,0,0,4,1,0,0,92,1,1,161,173,0,0).

	unserialize_checksum_mismatch_test() ->
		?assertEqual({error, checksum}, unserialize(<<123:8>>)),
		?assertEqual({error, invalid_data}, unserialize(<<1:8>>)).

	unserialize_corrupt_msg_test() ->
		{error, invalid_data} = unserialize(<<12, 54, 14 ,12, 15, 51,
		                                      69, 15, 19, 51, 91, 15>>).

	unserialize_payload_serial_test() ->
		Serial = <<"abc">>,
		{ok, Resp} = unserialize(<<21, ?BASEMSG, 16, 0, Serial/binary>>),
		?assertEqual(Serial, proplists:get_value(<<"serial">>, Resp)).

	unserialize_payload_event_test() ->
		BinV = 16#bb,
		{ok, Resp} = unserialize(<<35, ?BASEMSG, 02, 14, 0:32, 0, 0, 121, 187
		                         , 0, 0:16, 0:16, 2, 0, 1, 22>>),
		V = proplists:get_value(<<"voltage">>, Resp),
		?assertEqual(V, erlang:trunc((BinV*0.03)*100)*0.01).

	unserialize_payload_unknown_test() ->
		?assertEqual({error, invalid_data}, unserialize(<<35, ?BASEMSG, 20, 14
			, 0:32, 0, 0, 121, 187, 0, 0:16, 0:16, 2, 0, 1, 22>>)).

	serialize_serial_test() ->
		?assertEqual({ok, <<11, 1:32/little, 120, 17, "abcd">>}, serialize([
			  {<<"unique_id">>, 1}
			, {<<"type">>, <<"command">>}
			, {<<"command">>, <<"serial">>}
			, {<<"packet_number">>, 120}
			, {<<"data">>, <<"abcd">>}
		])).

	serialize_get_status_test() ->
		?assertEqual({ok, <<10, 1:32/little, 122, 3, 17, 0, 0>>}, serialize([
			  {<<"unique_id">>, 1}
			, {<<"type">>, <<"command">>}
			, {<<"command">>, <<"get_status">>}
			, {<<"packet_number">>, 122}
			])).

	serialize_get_config_test() ->
		?assertEqual({ok, <<10, 1:32/little, 123, 3, 19, 0, 0>>}, serialize([
			  {<<"unique_id">>, 1}
			, {<<"type">>, <<"command">>}
			, {<<"command">>, <<"get_config">>}
			, {<<"packet_number">>, 123}
			])).

	serialize_get_nid_test() ->
		?assertEqual({ok, <<10, 1:32/little, 123, 3, 16, 0, 0>>}, serialize([
			  {<<"unique_id">>, 1}
			, {<<"type">>, <<"command">>}
			, {<<"command">>, <<"get_nid">>}
			, {<<"packet_number">>, 123}
			])).

	serialize_set_output_test() ->
		Base = [
			  {<<"unique_id">>, 123}
			, {<<"type">>, <<"command">>}
			, {<<"command">>, <<"set_output">>}
			, {<<"packet_number">>, 124}
		],
		?assertEqual({error, no_output}, serialize(Base)),
		?assertEqual({ok, <<10, 123, 0, 0, 0, 124, 3, 1, 255, 0>>}, serialize(Base ++ [
			  {<<"output">>, [
				  {0, true}, {1, true}, {2, true}, {3, true}
				, {4, true}, {5, true}, {6, true}, {7, true}
			]} ])),
		?assertEqual({ok, <<10, 123, 0, 0, 0, 124, 3, 1, 0, 255>>}, serialize(Base ++ [
			  {<<"output">>, [
				  {0, false}, {1, false}, {2, false}, {3, false}
				, {4, false}, {5, false}, {6, false}, {7, false}
			]} ])),
		?assertEqual({ok, <<10, 123, 0, 0, 0, 124, 3, 1,2#00010010,2#000100001>>}
			, serialize(Base ++ [
			{<<"output">>, [
				  {0, false}, {1, true}
				, {4, true}, {5, false}
			]} ])).

	serialize_set_pwm_test() ->
		?assertEqual({error, missing_pwm}, serialize([
			  {<<"unique_id">>, 123}
			, {<<"type">>, <<"command">>}
			, {<<"command">>, <<"set_pwm">>}
			, {<<"packet_number">>, 125}
		])),
		?assertEqual({ok, <<10, 123, 0, 0, 0, 126, 3, 2, 50, 0>>}, serialize([
			  {<<"unique_id">>, 123}
			, {<<"type">>, <<"command">>}
			, {<<"command">>, <<"set_pwm">>}
			, {<<"packet_number">>, 126}
			, {<<"pwm">>, 50}
		])).

	serialize_error_test_() ->
		[ {"unknown type", ?_test(begin
			Payload = [
				  {<<"unique_id">>, 123}
				, {<<"type">>, <<"no-such-command">>}
				, {<<"command">>, unknown_command_atom}
				, {<<"packet_number">>, 127}],
			?assertEqual({error, msg_type}, serialize(Payload))
		  end)}
		, {"unknown command", ?_test(begin
			Payload = [
				  {<<"unique_id">>, 123}
				, {<<"type">>, <<"command">>}
				, {<<"command">>, <<"unknown_command_atom">>}
				, {<<"packet_number">>, 128}
			],
			?assertEqual({error, unknown_command}, serialize(Payload))
		  end)}
		, {"missing destination", ?_test(begin
			?assertEqual({error, msg_destination}, serialize([
				  {<<"type">>, <<"command">>}
				, {<<"packet_number">>, 129}
			]))
		  end)}
		].

	serialize_set_config_test() ->
		A = serialize([
			  {<<"unique_id">>, 123}
			, {<<"type">>, <<"command">>}
			, {<<"command">>, <<"set_config">>}
			, {<<"packet_number">>, 130}
			, {<<"config">>, [
				{max_jump_count, 2}
			]}
		]),
		?assertEqual({ok,<<16#28, 123, 0, 0, 0, 130, 3, 3, 10, 2, 0:30/unit:8>>}, A).

	serialize_test() ->
		Payload  = [
			  {<<"unique_id">>, 2}
			, {<<"type">>, <<"command">>}
			, {<<"command">>, <<"serial">>}
			, {<<"packet_number">>, 131}
			, {<<"data">>, <<"abcd">>}
		],
		Payload2 = [
			  {<<"type">>, <<"command">>}
			, {<<"unique_id">>, 3}
			, {<<"packet_number">>, 132}
			, {<<"command">>, <<"get_status">>}
		],
		?assertEqual({ok, <<11,2,0,0,0,131,17,$a,$b,$c,$d>>}, serialize(Payload)),
		?assertEqual({ok, <<10,3,0,0,0,132,3,17,0,0>>}, serialize(Payload2)).
		% Try serializing nothing
		%%serialize([]).
-endif.
