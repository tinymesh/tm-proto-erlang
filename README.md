# Tinymesh Erlang Parser

Erlang bindings for parsing the Tinymesh protocol.

Provides marshalling through `tinymesh:unserialize/{1,2}` and
`tinymesh:serialize/1`.

## Example

```erlang
%% Serializing a message
tinymesh:serialize([{<<"type">>, <<"command">>}
	, {<<"uid">>, 3}
	, {<<"cmd_number">>, 132}
	, {<<"type">>, <<"command">>}
	, {<<"command">>, <<"get_status">>}]).
{ok, [<<10,3,0,0,0,132,3,17,0,0>>]}

%% Unserializing a message
Buf = <<21,1,0,0,0,4,1,0,0,92,1,1,161,173,0,0,16,0,$a,$b,$c>>,
{ok, [Msg], _Rest} = tinymesh:unserialize(Buf).
{ok,[[{<<"sid">>,1}, ...}]], <<>>},

%% Unserializing framed messages
Part = <<21,1,0,0,0,4,1,0,0,92,1,1,161>>,
tinymesh:unserialize(<<173,0,0,16,0,$a,$b,$c>>, Part).
{ok, [[{<<"sid">>,1}, ....]]}
```

## Licensing

The code for the Workbench application is released under a 2-clause
BSD license. This license can be found in the `./LICENSE` file.
