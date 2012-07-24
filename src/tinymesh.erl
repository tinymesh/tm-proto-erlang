-module(tinymesh).

-export([unserialize/1]).

-type touplet() :: {Key :: atom(), Value :: any()}.
-type map()     :: [touplet(), ...].

-spec unserialize(binary()) -> List :: map().
unserialize(<<Checksum:8/unsigned-integer, _/binary>> = Data) when byte_size(Data) /= Checksum ->
	[{error, {checksum_mismatch, [{checksum, Checksum}, {size, byte_size(Data)}]}}];

unserialize(<<_Checksum:8/unsigned-integer,          % Message checksum
              SystemID:32/little-unsigned-integer,   % System wide ID
              UniqueID:32/little-unsigned-integer,   % Address for router
              RSSI:8/unsigned-integer,        % RSSI to first receiver
              NetworkLevel:8/unsigned-integer,% Vertical hops to gateway
              HopCounter:8/unsigned-integer,  % Actual hop count to gateway
              Counter:16/unsigned-integer,    % Unique message number
              Latency:16/unsigned-integer,    % Time since msg creation (10ms)
              Payload/binary>>) ->
	try
		lists:flatten([[{system_id,      SystemID},
		                {node_id,        UniqueID},
		                {rssi,           RSSI},
		                {jump_level,     NetworkLevel},
		                {jump_count,     HopCounter},
		                {msg_id,         Counter},
		                {packet_latency, Latency}] | unserialize_payload(Payload)])
	catch
			error: unknown_payload_type -> [{error, {unknown_payload_type, Payload}}]
	end;

unserialize(Data) ->
	[{error, {invalid_data, Data}}].

-spec unserialize_payload(Payload :: binary()) -> map().
unserialize_payload(<<16:8/unsigned-integer, 0:8, Payload/binary>>) ->
	[{type, serial}, {serial, base64:encode(Payload)}];

unserialize_payload(<<2:8/unsigned-integer,            %% Constant for event msg
                      Detail:8/unsigned-integer,       %% The message detail
                      MessageData:16/binary-unit:1,    %% User selected data
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
		{digital_io_0, DigitalIO_0}, {digital_io_1, DigitalIO_1},
		{digital_io_2, DigitalIO_2}, {digital_io_3, DigitalIO_3},
		{digital_io_4, DigitalIO_4}, {digital_io_5, DigitalIO_5},
		{digital_io_6, DigitalIO_6}, {digital_io_7, DigitalIO_7},
		{analog_io_0, AnalogIO_0},   {analog_io_1, AnalogIO_1},
		{hw_version,  HWVersion},    {fw_version,  FWVersion}];

unserialize_payload(_) ->
	erlang:error(unknown_payload_type).

-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").

	unserialize_checksum_mismatch_test() ->
		[{error, {checksum_mismatch, _}}] = unserialize(<<123:8>>).

	unserialize_corrupt_msg_test() ->
		[{error, {invalid_data, _}}] = unserialize(<<12, 54, 14 ,12, 15, 51,
		                                             69, 15, 19, 51, 91, 15>>).

	unserialize_payload_serial_test() ->
		Serial = <<"abc">>,
		Resp = unserialize_payload(<<16, 0, Serial/binary>>),
		[SerialData] = [ A || {serial, A} <- Resp],
		?assert(base64:decode(SerialData) == Serial).

	unserialize_payload_serial_no_constant_test() ->
		[{error, {unknown_payload_type, _}}] = unserialize(<<20, 1:32, 2:32, 19,
		                                                     1,  1,    1:16, 0:16,
		                                                     16, "abc">>).

	unserialize_payload_event_test() ->
		BinV = 16#bb,
		Resp = unserialize_payload(<<16#02, 16#0e, 16#00, 16#0, 16#00, 16#00, 16#0, 16#0,
		                             16#79, BinV,  16#00, 16#0, 16#00, 16#00, 16#0, 16#2,
		                             16#00, 16#01, 16#22>>),
		[Voltage] = [ A || {voltage, A} <- Resp],
		?_assert(Voltage == list_to_float(lists:flatten(io_lib:format("~.2f", [BinV])))).

	unserialize_payload_unknown_test() ->
		[{error, {unknown_payload_type, _}}] = unserialize(<<20, 1:32, 2:32, 19,
		                                                     1,  1,    1:16, 0:16,
		                                                     99, "abc">>).

-endif.
