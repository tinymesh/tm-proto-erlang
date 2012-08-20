-module(tinymesh).

-export([unserialize/1, serialize/1, handle_string/2]).
-compile([export_all]).
-type touplet() :: {Key :: atom(), Value :: any()}.
-type map()     :: [touplet(), ...].

-spec unserialize(binary()) -> List :: {Status :: atom(), Data :: (map() | {atom(), any()})}.
unserialize(<<Checksum:8/unsigned-integer, _/binary>> = Data) when byte_size(Data) /= Checksum ->
	{error, {checksum_mismatch, [{checksum, Checksum}, {size, byte_size(Data)}]}};

unserialize(<<_Checksum:8/unsigned-integer,          % Message checksum
              SystemID:32/little-unsigned-integer,   % System wide ID
              UniqueID:32/little-unsigned-integer,   % Address for router
              RSSI:8/unsigned-integer,        % RSSI to first receiver
              NetworkLevel:8/unsigned-integer,% Vertical hops to gateway
              HopCounter:8/unsigned-integer,  % Actual hop count to gateway
              Counter:16/unsigned-integer,    % Unique message number
              Latency:16/unsigned-integer,    % Time since msg creation (10ms)
              Payload/binary>> = Binary) ->
	try
		{ok, lists:flatten(
			[[ {raw,            base64:encode(Binary)}
			 , {system_id,      SystemID}
			 , {node_id,        UniqueID}
			 , {rssi,           RSSI}
			 , {jump_level,     NetworkLevel}
			 , {jump_count,     HopCounter}
			 , {msg_id,         Counter}
			 , {packet_latency, Latency}] | unserialize_payload(Payload)])}
	catch
			error: unknown_payload_type -> {error, {unknown_payload_type, Payload}}
	end;

unserialize(Data) ->
	{error, {invalid_data, Data}}.

-spec unserialize_payload(Payload :: binary()) -> map().
unserialize_payload(<<16:8/unsigned-integer, 0:8, Payload/binary>>) ->
	[{type, serial}, {serial, base64:encode(Payload)}];

unserialize_payload(<<2:8/unsigned-integer,            %% Constant for event msg
                      Detail:8/unsigned-integer,       %% The message detail
                      MessageData:16/integer-unit:1,   %% User selected data
                      LocatorID:32/unsigned-integer,   %% Locator ID, used for auth
                      Temperatur:8/unsigned-integer,   %% Module temp, always -128
                      Voltage:8/unsigned-integer,      %% Voltage monitor (V*0.03)
                      DigitalIO_0:1/unsigned-integer,  %% GPIO 0 status
                      DigitalIO_1:1/unsigned-integer,  %% GPIO 1 status
                      DigitalIO_2:1/unsigned-integer,  %% GPIO 2 status
                      DigitalIO_3:1/unsigned-integer,  %% GPIO 3 status
                      DigitalIO_4:1/unsigned-integer,  %% GPIO 4 status
                      DigitalIO_5:1/unsigned-integer,  %% GPIO 5 status
                      DigitalIO_6:1/unsigned-integer,  %% GPIO 6 status
                      DigitalIO_7:1/unsigned-integer,  %% GPIO 7 status
                      AnalogIO_0:16/unsigned-integer,  %% Analog IO 0
                      AnalogIO_1:16/unsigned-integer,  %% Analog IO 1
                      HWVersion:16/binary-unit:1,   %% Hardware revisions
                      FWVersion:16/binary-unit:1>>) ->
	<<HWVerMajor:8/unsigned-integer, HWVerMin:8/unsigned-integer>> = HWVersion,
	HWVersion2 = HWVerMajor + (HWVerMin / 100),
	<<FWVerMajor:8/unsigned-integer, FWVerMin:8/unsigned-integer>> = FWVersion,
	FWVersion2 = FWVerMajor + (FWVerMin / 100),
	[	{type, event},
		{detail, Detail},
		{detail_e, case Detail of
			16#01 -> io_change;
			16#02 -> analog_io_0_change;
			16#03 -> analog_io_1_change;
			16#08 -> power_on;
			16#09 -> ima;
			16#0E -> zacima;
			16#10 -> ack;
			16#11 -> not_accetable;
			16#21 -> get_config;
			_     -> erlang:error(unknown_message_detail)
		end},
		{msg_data,    MessageData},
		{locator,     LocatorID},
		{temp,        Temperatur - 128},
		{voltage,     list_to_float(lists:flatten(io_lib:format("~.2f", [Voltage * 0.03])))},
		{digital_io_0, 1 bxor DigitalIO_0}, {digital_io_1, 1 bxor DigitalIO_1},
		{digital_io_2, 1 bxor DigitalIO_2}, {digital_io_3, 1 bxor DigitalIO_3},
		{digital_io_4, 1 bxor DigitalIO_4}, {digital_io_5, 1 bxor DigitalIO_5},
		{digital_io_6, 1 bxor DigitalIO_6}, {digital_io_7, 1 bxor DigitalIO_7},
		{analog_io_0, AnalogIO_0},    {analog_io_1, AnalogIO_1},
		{hw_version,  HWVersion2},    {fw_version,  FWVersion2}];

unserialize_payload(_) ->
	erlang:error(unknown_payload_type).

-spec serialize(Payload :: map()) -> binary().
serialize(Payload) ->
	Type        = keyorerror(type,    Payload, missing_msg_type),
	Destination = keyorerror(node_id, Payload, missing_msg_destination),
	serialize(Destination, Type, Payload).

-spec serialize(Dest :: non_neg_integer(), Type :: atom(), Msg :: map()) -> binary().
serialize(Destination, command, Payload) ->
	MsgNumber = keyorerror(packet_number, Payload, missing_packet_number),
	Command   = keyorerror(command, Payload, missing_msg_command),
	serialize(Destination, command, MsgNumber, Command, Payload);

serialize(Destination, serial_out, Payload) ->
	serialize(Destination, serial, Payload);

serialize(Destination, serial, Payload) ->
	[MsgNumber] = [V || {packet_number, V} <- Payload],
	[Serial]    = [base64:decode(V) || {serial, V} <- Payload],
	Checksum    = 6 + byte_size(Serial),
	<<Checksum:8, Destination:32/little, MsgNumber:8, Serial/binary>>;

serialize(_Destination, _Type, _MsgNumber) when not is_atom(_Type) ->
	erlang:error(wrong_datatype_for_message_type).

-spec serialize(Dest :: non_neg_integer(), Type :: atom(), Command :: atom(),
                MsgNumber :: non_neg_integer(), Payload :: map()) -> binary().
serialize(Destination, command, MsgNumber, set_output, _) ->
	erlang:error(set_output_not_implemente),
	<<10, Destination:32/little, MsgNumber:8, 3, 1, 0, 0>>;

serialize(Destination, command, MsgNumber, set_pwm, _) ->
	erlang:error(set_pwm_not_implemented),
	<<10, Destination:32/little, MsgNumber:8, 3, 2, 0, 0>>;

serialize(Destination, command, MsgNumber, set_config, Payload) ->
	Config  = iolist_to_binary(lists:flatten(tinymesh_config:pack(Payload))),
	<<16#28, Destination:32/little, MsgNumber:8, 3, 3, Config/binary, 0:(32-byte_size(Config))/integer-unit:8>>;

serialize(Destination, command, MsgNumber, get_status, _) ->
	<<10, Destination:32/little, MsgNumber:8, 3, 17, 0, 0>>;

serialize(Destination, command, MsgNumber, get_config, _) ->
	<<10, Destination:32/little, MsgNumber:8, 3, 19, 0, 0>>;

serialize(_Destination, command, _MsgNumber, _Command, _) when is_atom(_Command) ->
	erlang:error(unknown_command_encountered);

serialize(_Destination, command, _MsgNumber, _, _) ->
	erlang:error(wrong_datatype_for_command).

-spec handle_string(Key :: atom(), Val :: string()) -> EncodedVal :: any().
handle_string(_Key, Val) when not is_list(Val) ->
	Val;

handle_string(Key, Val) when Key == command orelse Key == type ->
	list_to_atom(lists:flatten(Val));

handle_string(_, Val) ->
	list_to_integer(lists:flatten(Val)).

-spec keyorerror(Key :: atom(), List :: [{atom(), any()}], Error :: atom()) -> any().
keyorerror(Key, List, Error) ->
	case lists:keyfind(Key, 1, List) of
		{_, Val} -> Val;
		_        -> erlang:error(Error)
	end.

-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").

	unserialize_checksum_mismatch_test() ->
		{error, {checksum_mismatch, _}} = unserialize(<<123:8>>).

	unserialize_corrupt_msg_test() ->
		{error, {invalid_data, _}} = unserialize(<<12, 54, 14 ,12, 15, 51,
		                                             69, 15, 19, 51, 91, 15>>).

	unserialize_payload_serial_test() ->
		Serial = <<"abc">>,
		Resp = unserialize_payload(<<16, 0, Serial/binary>>),
		[SerialData] = [ A || {serial, A} <- Resp],
		?assert(base64:decode(SerialData) == Serial).

	unserialize_payload_serial_no_constant_test() ->
		{error, {unknown_payload_type, _}} = unserialize(<<20, 1:32, 2:32, 19,
		                                                     1,  1,    1:16, 0:16,
		                                                     16, "abc">>).

	unserialize_payload_event_test() ->
		BinV = 16#bb,
		Resp = unserialize_payload(<<16#02, 16#0e, 16#00, 16#0, 16#00, 16#00, 16#0, 16#0,
		                             16#79, BinV,  16#00, 16#0, 16#00, 16#00, 16#0, 16#2,
		                             16#00, 16#01, 16#22>>),
		[Voltage] = [ A || {voltage, A} <- Resp],
		?assert(Voltage == list_to_float(lists:flatten(io_lib:format("~.2f", [BinV * 0.03])))).

	unserialize_payload_unknown_test() ->
		{error, {unknown_payload_type, _}} = unserialize(<<20, 1:32, 2:32, 19,
		                                                     1,  1,    1:16, 0:16,
		                                                     99, "abc">>).

	serialize_serial_test() ->
		?assert(<<10, 1:32/little, 255, "abcd">> == serialize(1, serial, [
			{serial, base64:encode("abcd")}, {packet_number, 255}])).

	serialize_serial_out_link_test() ->
		?assert(<<10, 1:32/little, 255, "abcd">> == serialize(1, serial_out, [
			{serial, base64:encode("abcd")}, {packet_number, 255}])).

	serialize_get_status_test() ->
		?assert(<<10, 1:32/little, 254, 3, 17, 0, 0>> == serialize(1, command, 254, get_status, [])).

	serialize_get_config_test() ->
		?assert(<<10, 1:32/little, 253, 3, 19, 0, 0>> == serialize(1, command, 253, get_config, [])).

	serialize_set_output_test() ->
		e.

	serialize_set_pwm_test() ->
		e.

	serialize_unknown_command_test() ->
		Payload = [ {node_id, 123}, {type, command}, {command, unknown_command_atom},
		            {packet_number, 123}],
		?assertError(unknown_command_encountered, serialize(Payload)).

	serialize_wrong_datatype_for_msgtype_test() ->
		Payload = [ {node_id, 123}, {type, "command"}, {command, unknown_command_atom},
		            {packet_number, 123}],
		?assertError(wrong_datatype_for_message_type, serialize(Payload)).

	serialize_wrong_datatype_for_commandtype_test() ->
		Payload = [ {node_id, 123}, {type, command}, {command, "get_config"},
		            {packet_number, 123}],
		?assertError(wrong_datatype_for_command, serialize(Payload)).

	serialize_no_msg_type_test() ->
		?assertError(missing_msg_type, serialize([{a, b}, {c, d}, {e, f}])).

	serialize_invalid_type_test() ->
		Payload = [{type, "select"}, {node_id, 16#010101}, {packet_number, 100},
		           {serial, base64:encode("abcd")}],
		?assertError(wrong_datatype_for_message_type, serialize(Payload)).

	serialize_no_destination_test() ->
		?assertError(missing_msg_destination, serialize([{type, b}, {c, d}, {e, f}])).

	serialize_set_config_test() ->
		A = serialize(1, command, 199, set_config,[{max_jump_count, 2}]),
		?assert(<<16#28, 1:32/little, 199, 3, 3, 10, 2, 0:30/unit:8>> == A).

	serialize_test() ->
		Payload  = [{type, serial}, {node_id, 16#010101}, {packet_number, 100},
		           {serial, base64:encode("abcd")}],
		Payload2 = [{type, command}, {node_id, 16#010101}, {packet_number, 101},
		            {command, get_status}],
		serialize(Payload),
		serialize(Payload2).
		% Try serializing nothing
		%%serialize([]).

	handle_string_test() ->
		lists:foreach(
			fun
			    ({Key, Val, exception, Expected}) ->
					?assertException(error, Expected, handle_string(Key, Val));
				%%({Key, Val, error, Expected}) ->
				%%	?assertError(Expected, handle_string(Key, Val));
			    ({Key, Val, Expected}) ->
					?assertEqual(handle_string(Key, Val), Expected)
			end, [
				{command, "valid", valid},
				{command, "^weirdCommand", '^weirdCommand'},
				{node_id, "123", 123},
				{node_id, "}^invalidNode", exception, badarg},
				{digital_io, "123", 123},
				{latency, 123, 123},
				{fw_version, "1.31", exception, badarg}
			]).
-endif.
