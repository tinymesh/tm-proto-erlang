-module(tinymesh_config).

-export([
	  index/1
	, pack/1
	, unpack/1
	, unpack/3
	, pack_val/1]).

-type key() ::   rf_channel | rf_power | rf_data_rate | protocol_mode
               | rssi_threshold  | rssi_assesment | hiam_time
               | ima_time | connect_check_time | max_jump_level
               | max_jump_count | max_packet_latency | rf_retry_limit
               | serial_timeout | device_type | gpio_0_config
               | gpio_1_config | gpio_2_config | gpio_3_config
               | gpio_4_config | gpio_5_config | gpio_6_config
               | gpio_7_config | gpio_0_trigger | gpio_1_trigger
               | gpio_2_trigger | gpio_3_trigger | gpio_4_trigger
               | gpio_5_trigger | gpio_6_trigger | gpio_7_trigger
               | input_debounce | gpio_0_hi_hi_triggerlevel
               | gpio_0_hi_lo_triggerlevel | gpio_0_lo_hi_triggerlevel
               | gpio_0_lo_lo_triggerlevel | gpio_0_sample_rate
               | gpio_1_hi_hi_triggerlevel | gpio_1_hi_lo_triggerlevel
               | gpio_1_lo_hi_triggerlevel | gpio_1_lo_lo_triggerlevel
               | gpio_1_sample_rate | cts_hold_time | locator
               | node_id | system_id | baud_rate | model | hw_version
               | fw_version | ima_on_connect | pwm_default.

-type cfgpair() :: {key(), binary() | integer()}.

-export_type([cfgpair/0]).

-define(CONFIGPARAMS, [
	{rf_channel,                0,  1, int},   {rf_power,                  1,  1, int},
	{rf_data_rate,              2,  1, int},   {protocol_mode,             3,  1, int},
	{rssi_threshold,            4,  1, int},   {rssi_assesment,            5,  1, int},
	{hiam_time,                 6,  1, int},   {ima_time,                  7,  1, int},
	{connect_check_time,        8,  1, int},   {max_jump_level,            9,  1, int},
	{max_jump_count,            10, 1, int},   {max_packet_latency,        11, 1, int},
	{rf_retry_limit,            12, 1, int},   {serial_timeout,            13, 1, int},
	{device_type,               14, 1, int},   {gpio_0_config,             16, 1, int},
	{gpio_1_config,             17, 1, int},   {gpio_2_config,             18, 1, int},
	{gpio_3_config,             19, 1, int},   {gpio_4_config,             20, 1, int},
	{gpio_5_config,             21, 1, int},   {gpio_6_config,             22, 1, int},
	{gpio_7_config,             23, 1, int},   {gpio_0_trigger,            24, 1, int},
	{gpio_1_trigger,            25, 1, int},   {gpio_2_trigger,            26, 1, int},
	{gpio_3_trigger,            27, 1, int},   {gpio_4_trigger,            28, 1, int},
	{gpio_5_trigger,            29, 1, int},   {gpio_6_trigger,            30, 1, int},
	{gpio_7_trigger,            31, 1, int},   {input_debounce,            32, 1, int},
	{gpio_0_hi_hi_triggerlevel, 33, 1, int},   {gpio_0_hi_lo_triggerlevel, 34, 1, int},
	{gpio_0_lo_hi_triggerlevel, 35, 1, int},   {gpio_0_lo_lo_triggerlevel, 36, 1, int},
	{gpio_0_sample_rate,        37, 1, int},   {gpio_1_hi_hi_triggerlevel, 38, 1, int},
	{gpio_1_hi_lo_triggerlevel, 39, 1, int},   {gpio_1_lo_hi_triggerlevel, 40, 1, int},
	{gpio_1_lo_lo_triggerlevel, 41, 1, int},   {gpio_1_sample_rate,        42, 1, int},
	{cts_hold_time,             43, 1, int},   {locator,                   44, 1, int},
	{node_id,                   45, 4, int},   {system_id,                 49, 4, int},
	{baud_rate,                 53, 1, int},   {model,                     60, 10, binary},
	{hw_version,                73, 3, vsn},   {fw_version,                75, 3, vsn},
	{ima_on_connect,            94, 1, int},   {pwm_default,               95, 1, int}]
).

-spec index(non_neg_integer()) -> {ok, key()} | {error, {not_found, integer()}}.
index(N) ->
	case [X || {_,Offset,Length,_} = X <-
	         ?CONFIGPARAMS, Offset =< N, N + 1 - Length =< Offset ] of
		[{Key,_,_,_}] ->
			{ok, Key};
		[] -> {error, {not_found, N}}
	end.

-spec unpack(binary()) -> list(cfgpair()).
unpack(Data) ->
	unpack(Data,[], 0).

-spec unpack(binary(), list(cfgpair()), non_neg_integer()) -> list(cfgpair()).
unpack(<<>>, Acc,  _) ->
	lists:reverse(Acc);

unpack(Data, Acc, N) ->
	case [X || {_,Offset,Length,_} = X <-
	         ?CONFIGPARAMS, Offset =< N, N + 1 - Length =< Offset ] of
		[{Key, _, Len, Type}] ->
			{Tail, Val} = case Type of
				int ->
					<<Val0:Len/little-unsigned-integer-unit:8, Tail0/binary>> = Data,
					{Tail0, Val0};
				binary ->
					<<Val0:Len/binary-unit:8, Tail0/binary>> = Data,
					{Tail0, Val0};
				vsn ->
					<<Maj:8/integer, Min0:8/integer, Min1:8/integer, Tail0/binary>> = Data,
					{Tail0, <<((Maj + 48)), $., ((Min0 + 48)), ((Min1 + 48))>>}
				end,
			unpack(Tail, [{Key, Val}|Acc], N + Len);
		[] ->
			%% If this happends, config is invalid and the rest of the config
			%% should be disregarded ACK-INVALID
			<<_:8, Tail/binary>> = Data,
			unpack(Tail, Acc, N + 1)
	end.

pack(Config) ->
	pack(Config, []).

pack([], Acc)             -> lists:reverse(Acc);
pack([Cur | Config], Acc) ->
	case pack_val(Cur) of
		[]     -> pack(Config, Acc);
		Packed -> pack(Config, [Packed | Acc])
	end.


pack_val({Key, Value}) when is_binary(Key) ->
	pack_val({binary_to_existing_atom(Key, utf8), Value});
pack_val({_, Value}) when is_integer(Value), Value < 0 ->
	[];
pack_val({Key, Value}) when is_list(Value) ->
	pack_val({Key, list_to_binary(Value)});
pack_val({Key, Value}) when is_integer(Value) ->
	pack_val({Key, binary:encode_unsigned(Value)});
pack_val({Key, Value}) when is_binary(Value) ->
	ValueSize = byte_size(Value),
	case lists:keyfind(Key, 1, ?CONFIGPARAMS) of
	{_, Address, Length, _} when ValueSize =< Length ->
		{Res, _} = lists:mapfoldl(
			  fun(X, Acc) -> {[Acc, X], Acc+1} end
			, Address
			, binary_to_list(<<0:((Length-ValueSize)*8), Value/binary>>)),
		Res;
	_ ->
		[]
	end.

-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").

	unpack_test() ->
		%% Test unpacking a single integer value
		?assertEqual([{rf_channel, 1}], unpack(<<1:8>>)),
		%% Test float value, it defaults to 2-digit precision
		?assertEqual([{hw_version, <<"1.23">>}], unpack(<<1,2,3>>, [], 75)),
		%% Test string value
		?assertEqual([{model, <<"model123">>}], unpack(<<"model123">>, [], 60)),
		%% Validate 1 whole config
		Config = [ 16#01, 16#00, 16#00, 16#00, 16#02, 16#01, 16#00, 16#00, 16#83,
			16#01, 16#01, 16#00, 16#06, 16#00, 16#06, 16#02, 16#21, 16#07, 16#05,
			16#05, 16#00, 16#be, 16#c1, 16#04, 16#ff, 16#04, 16#14, 16#1e, 16#05,
			16#0a, 16#14, 16#02, 16#00, 16#01, 16#01, 16#01, 16#01, 16#01, 16#01,
			16#01, 16#01, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
			16#0a, 16#07, 16#ff, 16#00, 16#00, 16#0a, 16#07, 16#ff, 16#00, 16#00,
			16#0a, 16#19, 16#00, 16#02, 16#01, 16#00, 16#00, 16#01, 16#00, 16#00,
			16#00, 16#05, 16#01, 16#00, 16#00, 16#00, 16#00, 16#00, 16#52, 16#43,
			16#31, 16#31, 16#37, 16#30, 16#2d, 16#54, 16#4d, 16#2c, 16#32, 16#2e,
			16#30, 16#30, 16#2c, 16#31, 16#2e, 16#33, 16#31, 16#ff, 16#ff, 16#02,
			16#00, 16#01, 16#00, 16#00, 16#05, 16#00, 16#00, 16#00, 16#00, 16#01,
			16#00, 16#00, 16#00, 16#00, 16#01, 16#00, 16#00, 16#00, 16#00, 16#00,
			16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#52, 16#46
		],

		?assertEqual(52, length(unpack(list_to_binary(Config)))).

	pack_val_test() ->
		%% Test data-integrity loss
		?assertEqual([], pack_val({non_existing_config_parameter, 12345})),
		?assertEqual([], pack_val({rf_power, -1})),
		%% Test single byte fields are packed
		?assertEqual([[1, 20]], pack_val({rf_power, 20})),
		?assertEqual([], pack_val({rf_power, 16#F0FF})),

		%% Test multi-byte fields are packed correctly
		M = 16#FF,
		[P] = [B || {hw_version, B, _, _} <- ?CONFIGPARAMS],
		?assertEqual([[P,0], [P+1,1], [P+2,1]], pack_val({hw_version, 16#101})),
		?assertEqual([[P,2], [P+1,0], [P+2,1]], pack_val({hw_version, 16#20001})),
		?assertEqual([[P,M], [P+1,M], [P+2,M]], pack_val({hw_version, 16#FFFFFF})),
		?assertEqual([], pack_val({hw_version, 16#FFFFFFFF})),

		%% Test string fields
		StringMatch = [[A, 0]|| A <- lists:seq(60, 64)]
		               ++ [[65, $a], [66, $b], [67, $c]],
		?assertEqual(StringMatch, pack_val({model, "abc"})).

	pack_test() ->
		?assert([[[0,1]], [[1,2]], [[2,2]], [[3,1]]] == pack(
			[{rf_channel,    1},
		   {rf_power,      2},
		   {rf_data_rate,  2},
		   {protocol_mode, 1}
		])),

		%% Assert non existing parameters is not included
		?assert([[[0,1]], [[1,2]], [[3,1]]] == pack(
			[{rf_channel,    1},
		   {rf_power,      2},
		   {non_existing,  2},
		   {protocol_mode, 1}
		])),

		%% Assert out of bounds variables is not included
		?assert([[[0,1]], [[1,2]]] == pack(
			[{rf_channel,   1},
		   {rf_power,     2},
		   {rf_power,     -1},
		   {hw_version,   16#FFFFFFF}
		])).
-endif.
