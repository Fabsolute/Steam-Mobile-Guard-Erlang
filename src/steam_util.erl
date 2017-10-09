-module(steam_util).
%% API
-export([hex_to_bin/1, hex_to_list/1]).

hex_to_bin(HexString) ->
  list_to_binary(hex_to_list(HexString)).

hex_to_list([]) ->
  [];
hex_to_list([X, Y | T]) ->
  [to_int(X) * 16 + to_int(Y) | hex_to_list(T)].

to_int(Char) when $0 =< Char, Char =< $9 ->
  Char - $0;
to_int(Char) when $A =< Char, Char =< $F ->
  Char - $A + 10;
to_int(Char) when $a =< Char, Char =< $f ->
  Char - $a + 10.