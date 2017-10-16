-module(steam_util).

-include("../include/rsa_public_key.hrl").

%% API
-export([hex_to_bin/1, hex_to_list/1, encrypt_password/3]).

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

generate_public_key(Modulus, PublicExponent) ->
  #'RSAPublicKey'{modulus = binary_to_integer(Modulus, 16), publicExponent = binary_to_integer(PublicExponent, 16)}.

encrypt_password(Password, Modulus, PublicExponent) ->
  public_key:encrypt_public(Password, generate_public_key(Modulus, PublicExponent)).
