%%%-------------------------------------------------------------------
%%% @author fabsolutely
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Dec 2016 20:03
%%%-------------------------------------------------------------------
-author("fabsolutely").

-define(STEAM_API_BASE, "https://api.steampowered.com").
-define(STEAM_COMMUNITY_BASE, "https://steamcommunity.com").
-define(STEAM_API_VERSION, "/v0001").
-define(MOBILE_AUTH_BASE, ?STEAM_API_BASE ++ "/IMobileAuthService").
-define(MOBILE_AUTH_GET_WG_TOKEN, ?MOBILE_AUTH_BASE ++ "/GetWGToken" ++ ?STEAM_API_VERSION).
-define(TWO_FACTOR_BASE, ?STEAM_API_BASE ++ "/ITwoFactorService").
-define(TWO_FACTOR_TIME_QUERY, ?TWO_FACTOR_BASE ++ "/QueryTime" ++ ?STEAM_API_VERSION).
