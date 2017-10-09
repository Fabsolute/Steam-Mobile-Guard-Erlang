-module(steam_guard_code_generator).

-export([generate_token_with_shared_secret/1]).
-define(STEAM_GUARD_CODE_TRANSLATION, "23456789BCDFGHJKMNPQRTVWXY").

%%%% Generate token with steam accounts shared secrets
generate_token_with_shared_secret(Secret) ->
  Time = steam_time_aligner:get_steam_time(),
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
    _ -> generate_time_array(Time bsr 8, [Time band 255 | Acc], Counter + 1)
  end.

generate_token_from_hashed_data(HashedData) ->
  Byte = lists:nth(20, HashedData) band 15,
  CodePoint = ((lists:nth(Byte + 1, HashedData) band 127) bsl 24)
    bor ((lists:nth(Byte + 2, HashedData) band 255) bsl 16)
    bor ((lists:nth(Byte + 3, HashedData) band 255) bsl 8)
    bor (lists:nth(Byte + 4, HashedData) band 255),

  generate_token_from_hashed_data(HashedData, CodePoint, [], 0).

generate_token_from_hashed_data(HashedData, CodePoint, Acc, Counter) ->
  case Counter of
    5 -> Acc;
    _ -> Length = length(?STEAM_GUARD_CODE_TRANSLATION),
      Selected = lists:nth((CodePoint rem Length) + 1, ?STEAM_GUARD_CODE_TRANSLATION),
      generate_token_from_hashed_data(HashedData, trunc(CodePoint / Length),
        Acc ++ [Selected], Counter + 1)
  end.
