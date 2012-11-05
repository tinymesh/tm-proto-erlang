-module(tinymesh).

-export([unserialize/1, serialize/1]).

-type buf()     :: binary().
-type stream()  :: binary().
-type parsed()  :: [{binary(), binary() | integer()}].

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
serialize(_) -> [].

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
%
%	serialize_serial_test() ->
%		?assert(<<10, 1:32/little, 255, "abcd">> == serialize(1, serial, [
%			{serial, base64:encode("abcd")}, {packet_number, 255}])).
%
%	serialize_serial_out_link_test() ->
%		?assert(<<10, 1:32/little, 255, "abcd">> == serialize(1, serial_out, [
%			{serial, base64:encode("abcd")}, {packet_number, 255}])).
%
%	serialize_get_status_test() ->
%		?assert(<<10, 1:32/little, 254, 3, 17, 0, 0>> == serialize(1, command, 254, get_status, [])).
%
%	serialize_get_config_test() ->
%		?assert(<<10, 1:32/little, 253, 3, 19, 0, 0>> == serialize(1, command, 253, get_config, [])).
%
%	serialize_set_output_test() ->
%		e.
%
%	serialize_set_pwm_test() ->
%		e.
%
%	serialize_unknown_command_test() ->
%		Payload = [ {node_id, 123}, {type, command}, {command, unknown_command_atom},
%		            {packet_number, 123}],
%		?assertError(unknown_command_encountered, serialize(Payload)).
%
%	serialize_wrong_datatype_for_msgtype_test() ->
%		Payload = [ {node_id, 123}, {type, "command"}, {command, unknown_command_atom},
%		            {packet_number, 123}],
%		?assertError(wrong_datatype_for_message_type, serialize(Payload)).
%
%	serialize_wrong_datatype_for_commandtype_test() ->
%		Payload = [ {node_id, 123}, {type, command}, {command, "get_config"},
%		            {packet_number, 123}],
%		?assertError(wrong_datatype_for_command, serialize(Payload)).
%
%	serialize_no_msg_type_test() ->
%		?assertError(missing_msg_type, serialize([{a, b}, {c, d}, {e, f}])).
%
%	serialize_invalid_type_test() ->
%		Payload = [{type, "select"}, {node_id, 16#010101}, {packet_number, 100},
%		           {serial, base64:encode("abcd")}],
%		?assertError(wrong_datatype_for_message_type, serialize(Payload)).
%
%	serialize_no_destination_test() ->
%		?assertError(missing_msg_destination, serialize([{type, b}, {c, d}, {e, f}])).
%
%	serialize_set_config_test() ->
%		A = serialize(1, command, 199, set_config,[{max_jump_count, 2}]),
%		?assert(<<16#28, 1:32/little, 199, 3, 3, 10, 2, 0:30/unit:8>> == A).
%
%	serialize_test() ->
%		Payload  = [{type, serial}, {node_id, 16#010101}, {packet_number, 100},
%		           {serial, base64:encode("abcd")}],
%		Payload2 = [{type, command}, {node_id, 16#010101}, {packet_number, 101},
%		            {command, get_status}],
%		serialize(Payload),
%		serialize(Payload2).
%		% Try serializing nothing
%		%%serialize([]).
-endif.
