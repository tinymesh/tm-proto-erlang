-module(tinymesh).

-export([
	  unserialize/1
	, unserialize/2
	, serialize/1
	, handshake/1
	, ack/0
	]).

-type buf() :: binary().
-type stream() :: binary().
-type msg_ext() :: [{binary(), non_neg_integer() | float() | binary()}].
-type msg() :: [ checksum() | system_id() | unique_id() | rssi()
               | network_lvl() | jump_count() | packet_number()
               | latency() | type() | detail() | msg_data() | cmd_number()
               | address() | temperature() | voltage() | digital_io()
               | analog_io() | hardware() | firmware()].

-type checksum()      :: {checksum,      non_neg_integer()}.
-type system_id()     :: {system_id,     1..4294967295}.
-type unique_id()     :: {unique_id,     1..4294967295}.
-type rssi()          :: {rssi,          0..255}.
-type network_lvl()   :: {network_lvl,   0..255}.
-type jump_count()    :: {jump_count,    0..255}.
-type packet_number() :: {packet_number, 0..65535}.
-type cmd_number()    :: {cmd_number,    0..255}.
-type latency()       :: {latency,       0..65535}.
-type type()          :: {type,          binary()}.
-type detail()        :: {detail,        binary()}.
-type msg_data()      :: {msg_data,      0..65535}.
-type address()       :: {address,       0..4294967295}.
-type temperature()   :: {temperature,   0..128}.
-type voltage()       :: {voltage,       float()}.
-type digital_io()    :: { digital_io_0 | digital_io_1 | digital_io_2
                         | digital_io_3 | digital_io_4 | digital_io_5
                         | digital_io_6 | digital_io_7, 0 | 1}.
-type analog_io()     :: {analog_io_0 | analog_io_1, 0..65535}.
-type hardware()      :: {hardware,      <<_:32>>}.
-type firmware()      :: {firmware,      <<_:32>>}.

-define(match(V, K, M),
	{_, {<<K>>, V}} = {list_to_atom(K), lists:keyfind(<<K>>, 1, M)}).

%% Lookup integer key vs keyword
-define(table(Fun, Table),
Fun(N) when is_integer(N) ->
	{N, Arg} = lists:keyfind(N, 1, Table),
	Arg;
Fun(Arg) when is_binary(Arg) ->
	{N, Arg} = lists:keyfind(Arg, 2, Table),
	N
).

-define(cmd_args, [
	  {16#01, <<"set_output">>}
	, {16#02, <<"set_pwm">>}
	, {16#03, <<"set_config">>}
	, {16#05, <<"init_gw_config">>}
	, {16#10, <<"get_cid">>}
	, {16#11, <<"get_status">>}
	, {16#12, <<"get_did_status">>}
	, {16#13, <<"get_config">>}
	, {16#15, <<"force_reset">>}
	, {16#16, <<"get_path">>}
	]).

-define(power_triggers, [
	  {16#01, <<"power_on">>}
	, {16#02, <<"external">>}
	, {16#03, <<"sleep_or_config">>}
	, {16#04, <<"command">>}
	, {16#05, <<"watchdog">>}
	]).

-define(nack_reasons, [
	  {16#01, <<"packet_length">>}
	, {16#02, <<"gateway_config">>}
	, {16#03, <<"packet_format">>}
	, {16#0A, <<"node_config_length">>}
	, {16#0B, <<"node_config_command">>}
	, {16#0C, <<"node_packet_format">>}
	, {16#0D, <<"node_command_type">>}
	]).

-define(event_details, [
	  {16#01, <<"io_change">>}
	, {16#02, <<"analog_io_0_change">>}
	, {16#03, <<"analog_io_1_change">>}
	, {16#04, <<"temp_warning">>}
	, {16#05, <<"voltage_warning">>}
	, {16#05, <<"rf_tamper">>}
	, {16#08, <<"power_on">>}
	, {16#09, <<"ima">>}
	, {16#0A, <<"network_busy">>} %% co-existing with same system id
	, {16#0B, <<"network_inactiv">>}
	, {16#0C, <<"channel_jam">>} %% no cca
	, {16#0D, <<"shared_channel">>}
	, {16#0E, <<"zacima">>}
	, {16#10, <<"ack">>}
	, {16#11, <<"nack">>}
	, {16#12, <<"get_cid">>}
	, {16#20, <<"get_path">>}
	, {16#21, <<"get_config">>}
	]).

?table(cmd_arg, ?cmd_args).
?table(power_trigger, ?power_triggers).
?table(nack_reason, ?nack_reasons).
?table(event_detail, ?event_details).

-spec handshake(non_neg_integer()) -> binary().
handshake(PacketNumber) ->
	tinymesh:serialize([
		  {<<"unique_id">>,     0}
		, {<<"type">>,          <<"command">>}
		, {<<"command">>,       <<"get_cid">>}
		, {<<"packet_number">>, PacketNumber}
	]).

-spec ack() -> binary().
ack() ->
	{ok, <<6>>}.

-spec unserialize(stream()) -> {ok, [msg_ext()], buf()} | {error, Reason :: term()}.
unserialize(<<Chksum:8, _/binary>> = Buf) when byte_size(Buf) >= Chksum ->
	try proc(Buf) of
		{ok, Msgs} ->
			{ok, maptree(Msgs, fun map_elem/1)};
		{ok, Msgs, Rest} ->
			{ok, maptree(Msgs, fun map_elem/1), Rest};
		Res ->
			Res
	catch
		error:{badmatch, {config_index, {error, {not_found, N}}}} ->
			{error, {invalid_config_index, N}}
	end;
unserialize(<<Chksum:8, _/binary>> = Bin) when byte_size(Bin) < Chksum ->
	{error, checksum}.

-spec unserialize(stream(), Partial :: buf()) -> {ok, [msg()], buf()} | {error, Reason :: term()}.
unserialize(Buf, Partial) ->
	unserialize(<<Partial/binary, Buf/binary>>).

maptree(Tree, Fun) ->
	lists:map(fun
		({Key, Branch}) when is_list(Branch) ->
			Fun({Key, maptree(Branch, Fun)});
		(Branch) when is_list(Branch) ->
			maptree(Branch, Fun);
		(Leaf) ->
			Fun(Leaf)
	end, Tree).

map_elem({K, V}) when is_atom(K) ->
	{atom_to_binary(K, utf8), V};
map_elem(Ret) -> Ret.

-spec proc(buf()) -> {ok, list(msg()), binary()}.
proc(Buf) ->
	proc(Buf, []).

-spec proc(buf(), list(msg())) -> {ok, list(msg()), binary()}.
proc(<<Checksum:8/unsigned-integer,
       SystemID:32/little-unsigned-integer, % System wide ID
       UniqueID:32/little-unsigned-integer, % Device address
       RSSI:8/unsigned-integer,             % RSSI to first receiver
       NetworkLevel:8/unsigned-integer,     % Registered hops to gateway
       JumpCounter:8/unsigned-integer,      % Actual hops to gateway
       PacketCounter:16/unsigned-integer,
       Latency:16/unsigned-integer,         % Time since msg creation (10ms increment)
       16#02,                               % Message type: event message
       Detail0:8,
       Buf/binary>>, Acc) ->

	PayloadSize = Checksum - 18,
	<<Payload:PayloadSize/binary, Rest/binary>> = Buf,

	Detail = event_detail(Detail0),
	Msg0 = [
		  {system_id, SystemID}
		, {unique_id, UniqueID}
		, {rssi, RSSI}
		, {network_lvl, NetworkLevel}
		, {jump_count, JumpCounter}
		, {packet_number, PacketCounter}
		, {latency, Latency}
		, {type, <<"event">>}
		, {detail, Detail}
		],

	Msg = [Msg0 | expand_event(Detail, Payload)],
	proc(Rest, [Msg | Acc]);

%% event:serial
proc(<<Checksum:8/unsigned-integer,
       SystemID:32/little-unsigned-integer, % System wide ID
       UniqueID:32/little-unsigned-integer, % Device address
       RSSI:8/unsigned-integer,             % RSSI to first receiver
       NetworkLevel:8/unsigned-integer,     % Vertical hops to gateway
       JumpCounter:8/unsigned-integer,
       PacketCounter:16/unsigned-integer,
       Latency:16/unsigned-integer,         % Time since msg creation (10ms)
       16#10,                               % Message type: serial
       Seq:8/unsigned-integer,
       Buf/binary>>, Acc) ->

	PayloadSize = Checksum - 18,
	<<Payload:PayloadSize/binary, Rest/binary>> = Buf,

	Msg = [
		  {system_id, SystemID}
		, {unique_id, UniqueID}
		, {rssi, RSSI}
		, {network_lvl, NetworkLevel}
		, {jump_count, JumpCounter}
		, {packet_number, PacketCounter}
		, {latency, Latency}
		, {type, <<"serial">>}
		, {serial, Payload}
		, {sequence, Seq}
		],

	proc(Rest, [Msg | Acc]);

%% command:set_config
proc(<<_Checksum:8/unsigned-integer,
       UniqueID:32/little-unsigned-integer, % Destination address
       CmdNum:8/unsigned-integer,
       16#03,
       16#03,
       Config0:32/binary,
       Rest/binary>>, Acc) ->

	Config = lists:foldl(fun
		(K, {false, Cfg}) ->
			{K, Cfg};
	  (V, {K, Cfg}) ->
			{config_index, {ok, Key}} = {config_index, tinymesh_config:index(K)},
			{false, lists:keystore(K, 1, Cfg, {Key, K, V})}
	end, {false, []}, binary:bin_to_list(Config0)),

	Msg = [
		  {unique_id, UniqueID}
		, {cmd_number, CmdNum}
		, {type, <<"command">>}
		, {command, <<"set_config">>}
		, {config, Config}],

	proc(Rest, [Msg | Acc]);

%% command:*
proc(<<_Checksum:8/unsigned-integer,
       UniqueID:32/little-unsigned-integer, % Destination address
       CmdNum:8/unsigned-integer,
       16#03,
       Arg:8,
       Data1:8/binary-unit:1,
       Data2:8/binary-unit:1,
       Rest/binary>>, Acc) ->

	Msg = [
		  {unique_id, UniqueID}
		, {cmd_number, CmdNum}
		, {type, <<"command">>}
		| expand_cmd(Arg, Data1, Data2)],

	proc(Rest, [Msg | Acc]);

proc(<<_/binary>>, []) ->
	{error, invalid_data};

proc(<<>>, Acc) ->
	{ok, Acc};

proc(<<Rest/binary>>, Acc) ->
	{ok, Acc, Rest}.

expand_event(<<"get_path">>, <<_:8, UniqueID>>) ->
	[{path, UniqueID}];
expand_event(<<"get_config">>, Config) ->
	[{config, tinymesh_config:unpack(Config)}];
expand_event(Detail,
	<<MsgData:16/unsigned-integer,
	  Address:32/integer,
	  Temprature:8/signed-integer,
	  Voltage0:8/unsigned-integer,
	  DigitalIOs:8/binary-unit:1,
	  AnalogIO0:16/unsigned-integer,
	  AnalogIO1:16/unsigned-integer,
	  HwVersion:16/binary-unit:1,
	  FwVersion:16/binary-unit:1>>) ->

	Base = expand_event2(Detail, MsgData, Address),
	Voltage = erlang:trunc( (Voltage0 * 0.03) * 100) * 0.01,

	Base ++ [
		  {temprature, Temprature}
		, {voltage, Voltage}
		| [ digital_io(DigitalIOs)
		  | [ {analog_io_0, AnalogIO0}
		    , {analog_io_1, AnalogIO1}
		    , {hardware, version(HwVersion)}
		    , {firmware, version(FwVersion)}
		]]].

expand_event2(<<"rf_tamper">>, MsgData, Address) ->
	<<Duration:8/unsigned-integer, Ended:8/unsigned-integer>> = MsgData,
	[{duration, Duration}, {ended, Ended}, {locator, Address}];
expand_event2(<<"power_on">>, MsgData, Address) ->
	[{trigger, power_trigger(MsgData)}, {msg_data, MsgData}, {locator, Address}];
expand_event2(<<"ack">>, MsgData, Address) ->
	<<CmdNum:8, _:8>> = MsgData,
	[{cmd_number, CmdNum}, {msg_data, MsgData}, {locator, Address}];
expand_event2(<<"nack">>, MsgData, Address) ->
	<<CmdNum:8, Reason:8>> = MsgData,
	[ {cmd_number, CmdNum}, {reason, nack_reason(Reason)},
	  {msg_data, MsgData}, {locator, Address}];
expand_event2(_, MsgData, Address) -> [{msg_data, MsgData}, {locator, Address}].

%% Validate that this is done in correct order
expand_cmd(16#01 = Arg, SetGPIOs0, ClearGPIOs0) ->
	ClearGPIOs = [{K, 0} || {K, 1} <- digital_io(ClearGPIOs0)],
	SetGPIOs = [V || {_, 1} = V <- digital_io(SetGPIOs0)],

	Outputs = lists:ukeymerge(1, ClearGPIOs, SetGPIOs),

	[{command, cmd_arg(Arg)}, {outputs, Outputs}];
expand_cmd(16#02 = Arg, PWM, _) ->
	[{command, cmd_arg(Arg)}, {pwm, PWM}];
expand_cmd(Arg, _, _) -> [{command, cmd_arg(Arg)}].

-spec digital_io(<<_:8>>) -> [digital_io(), ...].
digital_io(<<D7:1, D6:1, D5:1, D4:1, D3:1, D2:1, D1:1, D0:1>>) ->
		[ {digital_io_0, D0}
		, {digital_io_1, D1}
		, {digital_io_2, D2}
		, {digital_io_3, D3}
		, {digital_io_4, D4}
		, {digital_io_5, D5}
		, {digital_io_6, D6}
		, {digital_io_7, D7}].

version(<<Major:8/integer, Min0:4/integer, Min1:4/integer>>) ->
	<<Major:8, $., Min0:8, Min1>>.

-spec serialize(msg()) -> {ok, [buf()]} | {error, Reason :: term()}.
serialize(Msg) ->
	try
		?match(Type, "type", Msg),
		serialize(Type, Msg)
	catch error:{badmatch, {T, false}} ->
					{error, {missing_field, T}}
	end.


serialize(<<"command">>, Msg) ->
	?match(Command, "command", Msg),
	?match(UniqueID, "unique_id", Msg),
	?match(CmdNum, "cmd_number", Msg),

	case Command of
		<<"serial">> ->
			?match(Serial, "data", Msg),
			{ok, [ <<((size(M) + 1)):8, M/binary>> || M <- build_serial(Serial, UniqueID, CmdNum, [])]};
		Command ->
			case serialize(<<"command">>, Command, Msg) of
				{error, _} = Err ->
					Err;
				Data ->
					Buf = <<UniqueID:32/little-integer, CmdNum/integer, 16#03, Data/binary>>,
					{ok, [<<((size(Buf) + 1)):8, Buf/binary>>]}
			end
	end;
serialize(_, _Msg) ->
	{error, msg_type}.

serialize(<<"command">>, <<"set_output">> = Arg, Msg) ->
	CmdArg = cmd_arg(Arg),
	?match(Outputs, "output", Msg),
	{O1, O2} = lists:foldl(fun fold_output/2, {0, 0}, Outputs),
	<<CmdArg/integer, O1/integer, O2/integer>>;

serialize(<<"command">>, <<"set_pwm">> = Arg, Msg) ->
	CmdArg = cmd_arg(Arg),
	?match(PWM, "pwm", Msg),
	<<CmdArg/integer, PWM/integer, 0>>;

serialize(<<"command">>, <<"set_config">> = Arg, Msg) ->
	CmdArg = cmd_arg(Arg),
	?match(Payload, "config", Msg),
	Config  = iolist_to_binary(lists:flatten(tinymesh_config:pack(Payload))),
	<<CmdArg/integer, Config/binary, 0:(32-byte_size(Config))/integer-unit:8>>;

serialize(<<"command">>, Command, _Msg) ->
	try cmd_arg(Command) of
		CmdArg ->
			<<CmdArg/integer, 0, 0>>
	catch error:{badmatch, false} ->
			{error, unknown_command}
	end.

fold_output({<<N>>, Val}, Acc) -> fold_output({N-48, Val}, Acc);
fold_output({N, true}, {On, Off}) -> {On + erlang:trunc(math:pow(2, N)), Off};
fold_output({N, 1},    {On, Off}) -> {On + erlang:trunc(math:pow(2, N)), Off};
fold_output({N, false},{On, Off}) -> {On, Off + erlang:trunc(math:pow(2, N))};
fold_output({N, 0},    {On, Off}) -> {On, Off + erlang:trunc(math:pow(2, N))}.


build_serial(<<Data/binary>>, UniqueID, CmdNum, Acc) when size(Data) =< 120 ->
	[<<UniqueID:32/little-integer, CmdNum/integer, 16#11, Data/binary>> | Acc];

build_serial(<<Data:120/binary, Rest/binary>>, UniqueID, CmdNum0, Acc0) ->
	CmdNum = (CmdNum0 + 1) rem 16#FF,
	Acc = [<<UniqueID:32/little-integer, CmdNum/integer, 16#11, Data/binary>> | Acc0],
	build_serial(Rest, UniqueID, CmdNum, Acc).

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
		{ok, [Resp]} = unserialize(<<21, ?BASEMSG, 16, 0, Serial/binary>>),
		?assertEqual(Serial, proplists:get_value(<<"serial">>, Resp)).

	unserialize_payload_event_test() ->
		BinV = 16#bb,
		{ok, [Resp]} = unserialize(<<35, ?BASEMSG, 02, 14, 0:32, 0, 0, 121, 187
		                         , 0, 0:16, 0:16, 2, 0, 1, 22>>),
		V = proplists:get_value(<<"voltage">>, Resp),
		?assertEqual(V, erlang:trunc((BinV*0.03)*100)*0.01).

	unserialize_payload_unknown_test() ->
		?assertEqual({error, invalid_data}, unserialize(<<35, ?BASEMSG, 20, 14
			, 0:32, 0, 0, 121, 187, 0, 0:16, 0:16, 2, 0, 1, 22>>)).

	serialize_serial_test() ->
		io:format("~n+a: ~p~n-b: ~p", [{ok, [<<11, 1:32/little, 120, 17, "abcd">>]}, serialize([
			  {<<"unique_id">>, 1}
			, {<<"type">>, <<"command">>}
			, {<<"command">>, <<"serial">>}
			, {<<"cmd_number">>, 120}
			, {<<"data">>, <<"abcd">>}
		])]),
		?assertEqual({ok, [<<11, 1:32/little, 120, 17, "abcd">>]}, serialize([
			  {<<"unique_id">>, 1}
			, {<<"type">>, <<"command">>}
			, {<<"command">>, <<"serial">>}
			, {<<"cmd_number">>, 120}
			, {<<"data">>, <<"abcd">>}
		])).

	serialize_get_status_test() ->
		?assertEqual({ok, [<<10, 1:32/little, 122, 3, 17, 0, 0>>]}, serialize([
			  {<<"unique_id">>, 1}
			, {<<"type">>, <<"command">>}
			, {<<"command">>, <<"get_status">>}
			, {<<"cmd_number">>, 122}
			])).

	serialize_get_config_test() ->
		?assertEqual({ok, [<<10, 1:32/little, 123, 3, 19, 0, 0>>]}, serialize([
			  {<<"unique_id">>, 1}
			, {<<"type">>, <<"command">>}
			, {<<"command">>, <<"get_config">>}
			, {<<"cmd_number">>, 123}
			])).

	serialize_get_cid_test() ->
		?assertEqual({ok, [<<10, 1:32/little, 123, 3, 16, 0, 0>>]}, serialize([
			  {<<"unique_id">>, 1}
			, {<<"type">>, <<"command">>}
			, {<<"command">>, <<"get_cid">>}
			, {<<"cmd_number">>, 123}
			])).

	serialize_set_output_test() ->
		Base = [
			  {<<"unique_id">>, 123}
			, {<<"type">>, <<"command">>}
			, {<<"command">>, <<"set_output">>}
			, {<<"cmd_number">>, 124}
		],
		?assertEqual({error, {missing_field, output}}, serialize(Base)),
		?assertEqual({ok, [<<10, 123, 0, 0, 0, 124, 3, 1, 255, 0>>]}, serialize(Base ++ [
			  {<<"output">>, [
				  {0, true}, {1, true}, {2, true}, {3, true}
				, {4, true}, {5, true}, {6, true}, {7, true}
			]} ])),
		?assertEqual({ok, [<<10, 123, 0, 0, 0, 124, 3, 1, 0, 255>>]}, serialize(Base ++ [
			  {<<"output">>, [
				  {0, false}, {1, false}, {2, false}, {3, false}
				, {4, false}, {5, false}, {6, false}, {7, false}
			]} ])),
		?assertEqual({ok, [<<10, 123, 0, 0, 0, 124, 3, 1,2#00010010,2#000100001>>]}
			, serialize(Base ++ [
			{<<"output">>, [
				  {0, false}, {1, true}
				, {4, true}, {5, false}
			]} ])).

	serialize_set_pwm_test() ->
		?assertEqual({error, {missing_field, pwm}}, serialize([
			  {<<"unique_id">>, 123}
			, {<<"type">>, <<"command">>}
			, {<<"command">>, <<"set_pwm">>}
			, {<<"cmd_number">>, 125}
		])),
		?assertEqual({ok, [<<10, 123, 0, 0, 0, 126, 3, 2, 50, 0>>]}, serialize([
			  {<<"unique_id">>, 123}
			, {<<"type">>, <<"command">>}
			, {<<"command">>, <<"set_pwm">>}
			, {<<"cmd_number">>, 126}
			, {<<"pwm">>, 50}
		])).

	serialize_error_test_() ->
		[ {"unknown type", ?_test(begin
			Payload = [
				  {<<"unique_id">>, 123}
				, {<<"type">>, <<"no-such-command">>}
				, {<<"command">>, unknown_command_atom}
				, {<<"cmd_number">>, 127}],
			?assertEqual({error, msg_type}, serialize(Payload))
		  end)}
		, {"unknown command", ?_test(begin
			Payload = [
				  {<<"unique_id">>, 123}
				, {<<"type">>, <<"command">>}
				, {<<"command">>, <<"unknown_command_atom">>}
				, {<<"cmd_number">>, 128}
			],
			?assertEqual({error, unknown_command}, serialize(Payload))
		  end)}
		, {"missing destination", ?_test(begin
			?assertEqual({error, {missing_field, unique_id}}, serialize([
				  {<<"type">>, <<"command">>}
				, {<<"cmd_number">>, 129}
				, {<<"command">>, serial}
			]))
		  end)}
		].

	serialize_set_config_test() ->
		A = serialize([
			  {<<"unique_id">>, 123}
			, {<<"type">>, <<"command">>}
			, {<<"command">>, <<"set_config">>}
			, {<<"cmd_number">>, 130}
			, {<<"config">>,
				[
					{<<"max_jump_count">>, 2}
				]}
		]),
		?assertEqual({ok, [<<16#28, 123, 0, 0, 0, 130, 3, 3, 10, 2, 0:30/unit:8>>]}, A).

	serialize_set_output_binary_test() ->
		Base = [
		  {<<"unique_id">>,     6}
		, {<<"type">>,          <<"command">>}
		, {<<"command">>,       <<"set_output">>}
		, {<<"cmd_number">>, 0}],
		?assertEqual({ok, [<<10,6,0,0,0,0,3,1,192,0>>]}
		, tinymesh:serialize([{<<"output">>,[{<<"6">>,true},{<<"7">>,true}]}|Base])).

	serialize_test() ->
		Payload  = [
			  {<<"unique_id">>, 2}
			, {<<"type">>, <<"command">>}
			, {<<"command">>, <<"serial">>}
			, {<<"cmd_number">>, 131}
			, {<<"data">>, <<"abcd">>}
		],
		Payload2 = [
			  {<<"type">>, <<"command">>}
			, {<<"unique_id">>, 3}
			, {<<"cmd_number">>, 132}
			, {<<"command">>, <<"get_status">>}
		],
		?assertEqual({ok, [<<11,2,0,0,0,131,17,$a,$b,$c,$d>>]}, serialize(Payload)),
		?assertEqual({ok, [<<10,3,0,0,0,132,3,17,0,0>>]}, serialize(Payload2)).
		% Try serializing nothing
		%%serialize([]).
-endif.
