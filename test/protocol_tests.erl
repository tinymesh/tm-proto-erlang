-module(protocol_tests).

-include_lib("eunit/include/eunit.hrl").

unserialize_test() ->
	{ok, Buf} = file:read_file("../test/data/messages.b64"),
	[ ?assertMatch(
		{ok, _}, tinymesh:unserialize( base64:decode(binary_to_list(X))))
			|| <<_:88, X/binary>> <- binary:split(Buf,  <<10>>, [global])].
