-module(tinymesh_config).

-type cfgtouplet() :: {Key :: atom(), Value :: any()}.
-type cfglist() :: [cfgbin(), ...].
-type cfgdef() :: {Key :: atom(),Address :: byte(),Length :: non_neg_integer()}.
-type cfgbin() :: binary().

-export_type([cfgdef/0]).

-export([pack/1, unpack/1, pack_val/1]).

-define(CONFIGPARAMS, [
	{rf_channel,                0,  1}, {rf_power,                  1,  1},
	{rf_data_rate,              2,  1}, {protocol_mode,             3,  1},
	{rssi_threshold,            4,  1}, {rssi_assesment,            5,  1},
	{hiam_time,                 6,  1}, {ima_time,                  7,  1},
	{connect_check_time,        8,  1}, {max_jump_level,            9,  1},
	{max_jump_count,            10, 1}, {max_packet_latency,        11, 1},
	{rf_retry_limit,            12, 1}, {serial_timeout,            13, 1},
	{device_type,               14, 1}, {gpio_0_config,             16, 1},
	{gpio_1_config,             17, 1}, {gpio_2_config,             18, 1},
	{gpio_3_config,             19, 1}, {gpio_4_config,             20, 1},
	{gpio_5_config,             21, 1}, {gpio_6_config,             22, 1},
	{gpio_7_config,             23, 1}, {gpio_0_trigger,            24, 1},
	{gpio_1_trigger,            25, 1}, {gpio_2_trigger,            26, 1},
	{gpio_3_trigger,            27, 1}, {gpio_4_trigger,            28, 1},
	{gpio_5_trigger,            29, 1}, {gpio_6_trigger,            30, 1},
	{gpio_7_trigger,            31, 1}, {input_debounce,            32, 1},
	{gpio_0_hi_hi_triggerlevel, 33, 1}, {gpio_0_hi_lo_triggerlevel, 34, 1},
	{gpio_0_lo_hi_triggerlevel, 35, 1}, {gpio_0_lo_lo_triggerlevel, 36, 1},
	{gpio_0_sample_rate,        37, 1}, {gpio_1_hi_hi_triggerlevel, 38, 1},
	{gpio_1_hi_lo_triggerlevel, 39, 1}, {gpio_1_lo_hi_triggerlevel, 40, 1},
	{gpio_1_lo_lo_triggerlevel, 41, 1}, {gpio_1_sample_rate,        42, 1},
	{cts_hold_time,             43, 1}, {locator,                   44, 1},
	{node_id,                   45, 4}, {system_id,                 49, 4},
	{baud_rate,                 53, 1}, {model,                     60, 8},
	{hw_version,                75, 3}, {fw_version,                77, 3},
	{ima_on_connect,            94, 1}, {pwm_default,               95, 1}]
).


-spec pack(Config :: [cfgtouplet()], Acc :: [cfgtouplet()]) -> iolist().
pack([], Acc)             -> Acc;
pack([Cur | Config], Acc) -> pack(Config, [pack(Cur) | Acc]).

-spec unpack(Config :: cfgbin()) -> (Config :: [cfgtouplet()]).
unpack(<<_:8, _:8>>) ->
	[].

-spec pack(Config :: [cfgtouplet()]) -> iolist().
pack(Config) ->
	pack(Config, []).

-spec pack_val(cfgtouplet()) -> cfglist().
pack_val({_, Value}) when is_integer(Value), Value < 0 ->
	[];

pack_val({Key, Value}) when not is_binary(Value) ->
	try
		case binary:encode_unsigned(Value) of
			A when is_binary(A) -> pack_val({Key, A});
			_                   -> unreached
		end
	catch _: _ -> pack_val({Key, list_to_binary(Value)})
	end;

pack_val({Key, Value}) when is_binary(Value) ->
	ValueSize = byte_size(Value),
	case lists:keyfind(Key, 1, ?CONFIGPARAMS) of
	{_, Address, Length} when ValueSize =< Length ->
		{Res, _} = lists:mapfoldl(fun(X, Acc) -> {[Acc, X], Acc+1} end,
		                          Address, binary_to_list(
		                           <<0:((Length-ValueSize)*8), Value/binary>>)),
		Res;
	_ ->
		[]
	end.

-spec config_lookup(non_neg_integer()) -> List :: [cfgdef()].
config_lookup(Pos) ->
	[{A, B, C} || {A, B, C} <- ?CONFIGPARAMS, B =< Pos, Pos + 1 - C =< B ].

-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").
	pack_val_test() ->
		%% Test data-integrity loss
		?assert([] == pack_val({non_existing_config_parameter, 12345})),
		?assert([] == pack_val({rw_power, -1})),
		%% Test single byte fields are packed
		?assert([[1, 20]] == pack_val({rf_power, 20})),
		?assert([]        == pack_val({rf_power, 16#F0FF})),

		%% Test multi-byte fields are packed correctly
		[P] = [B || {hw_version, B, _} <- ?CONFIGPARAMS],
		?assert([[P, 0], [P+1, 1], [P+2, 1]] == pack_val({hw_version, 16#000101})),
		?assert([[P, 2], [P+1, 0], [P+2, 1]] == pack_val({hw_version, 16#020001})),
		?assert([[P, 255], [P+1, 255], [P+2, 255]] == pack_val({hw_version, 16#FFFFFF})),
		?assert([] == pack_val({hw_version, 16#FFFFFFFF})), %% Pack 32bit into 24bit fails.

		%% Test string fields
		StringMatch = [[A, 0] || A <- lists:seq(60, 64)]
		               ++ [[65, $a], [66, $b], [67, $c]],
		?assert(StringMatch == pack_val({model, "abc"})).
-endif.

%%%%%%%%%%%%%%%%%%
%
%unserialize_val(<<Data/binary>>) ->
%	unserialize_val([], Data, 0).
%
%unserialize_val(Acc, A, _) when byte_size(A) =:= 0 ->
%	Acc;
%
%unserialize_val(Acc, Data, Offset) ->
%	case config_lookup(Offset) of
%		[{Key, _, Len}] ->
%			Len2 = Len*8,
%			<<Val:Len2/little, Tail/binary>> = Data,
%			unserialize_val([{Key, Val}|Acc], Tail, Offset + Len);
%		[] ->
%			<<_:8, Tail/binary>> = Data,
%			unserialize_val(Acc, Tail, Offset + 1)
%	end.
%
%
