-define(STEAM_API_BASE, "http://api.steampowered.com").
-define(STEAM_COMMUNITY_BASE, "http://steamcommunity.com").
-define(STEAM_API_VERSION, "/v0001").
-define(MOBILE_AUTH_BASE, ?STEAM_API_BASE ++ "/IMobileAuthService").
-define(MOBILE_AUTH_GET_WG_TOKEN, ?MOBILE_AUTH_BASE ++ "/GetWGToken" ++ ?STEAM_API_VERSION).
-define(TWO_FACTOR_BASE, ?STEAM_API_BASE ++ "/ITwoFactorService").
-define(TWO_FACTOR_TIME_QUERY, ?TWO_FACTOR_BASE ++ "/QueryTime" ++ ?STEAM_API_VERSION).
-define(OAUTH_PARAMETERS, "?oauth_client_id=DE45CD61&oauth_scope=read_profile%20write_profile%20read_client%20write_client").
-define(LOGIN_BASE, ?STEAM_COMMUNITY_BASE ++ "/login").
-define(LOGIN_OAUTH, ?LOGIN_BASE ++ ?OAUTH_PARAMETERS).
-define(LOGIN_GET_RSA_KEY, ?LOGIN_BASE ++ "/getrsakey").
-define(LOGIN_DO_LOGIN, ?LOGIN_BASE ++ "/dologin").
-define(MOBILE_LOGIN_BASE, ?STEAM_COMMUNITY_BASE ++ "/mobilelogin").
-define(MOBILE_LOGIN_OAUTH, ?MOBILE_LOGIN_BASE ++ ?OAUTH_PARAMETERS).
