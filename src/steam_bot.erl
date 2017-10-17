-module(steam_bot).

-include("../include/steam_urls.hrl").
-include("../include/http_response.hrl").

-export([
  start/0,
  start_link/0,
  login/2
]).

-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-behavior(gen_server).

-record(state, {
  bots = []
}).

-record(bot_state, {
  username = null,
  password = null
}).

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

login(Username, Password) ->
  gen_server:call(?MODULE, {login, Username, Password}).

%% gen_server

init([]) ->
  steam_cookie:add_cookie("mobileClientVersion", "3067969+%282.1.3%29"),
  steam_cookie:add_cookie("mobileClient", "android"),
  steam_cookie:add_cookie("Steam_Language", "english"),
  steam_cookie:add_cookie("dob", ""),
  steam_cookie:add_cookie("steamid", ""),
  steam_cookie:add_cookie("steamLogin", ""),
  steam_cookie:add_header("Accept", "text/javascript, text/html, application/xml, text/xml, */*"),
  steam_cookie:add_header("User-Agent", "Mozilla/5.0 (Linux; U; Android 4.1.1; en-us; Google Nexus 4 - 4.1.1 - API 16 - 768x1280 Build/JRO03S) AppleWebKit/534.30 (KHTML, like Gecko) Version/4.0 Mobile Safari/534.30"),
  steam_cookie:add_header("X-Requested-With", "com.valvesoftware.android.steam.community"),

  Response = steam_http:mobile_get(
    ?LOGIN_OAUTH,
    steam_cookie:get_cookies(),
    steam_cookie:get_headers()
  ),

  steam_cookie:add_cookies(Response#http_response.cookies),
  {ok, #state{}}.

handle_call({login, Username, Password}, _From, State) ->
  RSAPostData = [{<<"username">>, Username}],
  RSAResponse = steam_http:mobile_post(
    ?LOGIN_GET_RSA_KEY,
    RSAPostData,
    steam_cookie:get_cookies(Username),
    steam_cookie:get_headers(Username)
  ),
  RSABody = RSAResponse#http_response.json_body,
  EncryptedPassword =
    case maps:get(<<"success">>, RSABody) of
      false -> throw({error, bad_rsa});
      _ ->
        Exponent = maps:get(<<"publickey_exp">>, RSABody),
        Modulus = maps:get(<<"publickey_mod">>, RSABody),
        base64:encode(steam_util:encrypt_password(Password, Modulus, Exponent))
    end,
  steam_cookie:add_cookies(Username, RSAResponse#http_response.cookies),
  TwoFactorCode = steam_guard_code_generator:generate_token_with_shared_secret("XU/6LNWNIQOt9venDFEU7jj52uE="),
  RSATimestamp = maps:get(<<"timestamp">>, RSABody),
  LoginPostData = [
    {<<"username">>, Username},
    {<<"password">>, EncryptedPassword},
    {<<"twofactorcode">>, TwoFactorCode},
    {<<"rsatimestamp">>, RSATimestamp},
    {<<"emailauth">>, <<"">>},
    {<<"loginfriendlyname">>, <<"#login_emailauth_friendlyname_mobile">>},
    {<<"captchagid">>, <<"-1">>},
    {<<"captcha_text">>, <<"enter above characters">>},
    {<<"emailsteamid">>, <<"">>},
    {<<"remember_login">>, <<"false">>},
    {<<"oauth_client_id">>, <<"DE45CD61">>},
    {<<"oauth_scope">>, <<"read_profile write_profile read_client write_client">>},
    {<<"donotcache">>, integer_to_binary(steam_time_aligner:get_local_time())}
  ],
  LoginResponse = steam_http:mobile_post(
    ?LOGIN_DO_LOGIN,
    LoginPostData,
    steam_cookie:get_cookies(Username),
    steam_cookie:get_headers(Username)
  ),

  steam_cookie:add_cookies(Username, LoginResponse#http_response.cookies),
  {reply, {rsa, RSAResponse, login, LoginResponse}, State};

handle_call(Request, From, State) ->
  erlang:error(not_implemented).

handle_cast(Request, State) ->
  erlang:error(not_implemented).

handle_info(Info, State) ->
  erlang:error(not_implemented).

terminate(Reason, State) ->
  ok.

code_change(OldVsn, State, Extra) ->
  erlang:error(not_implemented).