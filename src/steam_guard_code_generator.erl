-module(steam_guard_code_generator).

-export([start/0]).
-export([stop/0]).
-export([generate_token_with_shared_secret/1]).
-define(STEAM_GUARD_CODE_TRANSLATION, "23456789BCDFGHJKMNPQRTVWXY").

start() ->
  ensure_started(crypto),
  ensure_started(asn1),
  ensure_started(public_key),
  ensure_started(ssl),
  ensure_started(inets),
  ensure_started(steam_guard_code_generator),
  steam_guard_code_time_aligner:start().

stop() -> ok.

ensure_started(App) ->
  case application:start(App) of
    ok -> ok;
    {error, {already_started, App}} -> ok
  end.

%%%% Generate token with steam accounts shared secrets
generate_token_with_shared_secret(Secret) ->
  Time = steam_guard_code_time_aligner:get_steam_time(),
  generate_token_with_shared_secret_and_time(Secret, Time).

generate_token_with_shared_secret_and_time(Secret, Time) ->
  SharedSecretArray = base64:decode(Secret),
  TimeArray = generate_time_array(Time),
  HashedData = binary_to_list(crypto:hmac(sha, SharedSecretArray, TimeArray)),
  CodeArray = generate_token_from_hashed_data(HashedData),
  CodeArray.

generate_time_array(Time) -> generate_time_array(trunc(Time / 30), [], 0).

generate_time_array(Time, Acc, Counter) ->
  case Counter of
    8 -> Acc;
    _ -> generate_time_array(Time bsr 8, [Time band 255] ++ Acc, Counter + 1)
  end.

generate_token_from_hashed_data(HashedData) ->
  Byte = lists:nth(20, HashedData) band 15,
  CodePoint = ((lists:nth(Byte + 1, HashedData) band 127) bsl 24) bor ((lists:nth(Byte + 2, HashedData) band 255) bsl 16)
    bor ((lists:nth(Byte + 3, HashedData) band 255) bsl 8) bor (lists:nth(Byte + 4, HashedData) band 255),
  generate_token_from_hashed_data(HashedData, CodePoint, [], 0).

generate_token_from_hashed_data(HashedData, CodePoint, Acc, Counter) ->
  case Counter of
    5 -> Acc;
    _ -> Length = length(?STEAM_GUARD_CODE_TRANSLATION),
      Selected = lists:nth((CodePoint rem Length) + 1, ?STEAM_GUARD_CODE_TRANSLATION),
      generate_token_from_hashed_data(HashedData, trunc(CodePoint / Length),
        Acc ++ [Selected], Counter + 1)
  end.
