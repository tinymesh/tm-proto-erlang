-module(protocol_tests).

-include_lib("eunit/include/eunit.hrl").

unserialize_test_() ->
	{generator, fun() ->
		{ok, Buf} = file:read_file("../test/data/messages.b64"),
		[ ?_test(begin
			?assertMatch(
				{ok, _}, tinymesh:unserialize( base64:decode(binary_to_list(X))))
		  end) || <<_:88, X/binary>> <- binary:split(Buf,  <<10>>, [global])]
	 end}.
