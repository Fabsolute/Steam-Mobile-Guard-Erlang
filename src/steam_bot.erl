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
  Cookies = [
    {"mobileClientVersion", "0213"},
    {"mobileClient", "android"},
    {"Steam_Language", "english"}
  ],
  Headers = [
    {"Accept", "text/javascript, text/html, application/xml, text/xml, */*"},
    {"User-Agent", "Mozilla/5.0 (Linux; U; Android 4.1.1; en-us; Google Nexus 4 - 4.1.1 - API 16 - 768x1280 Build/JRO03S) AppleWebKit/534.30 (KHTML, like Gecko) Version/4.0 Mobile Safari/534.30"}
  ],
  Response = steam_http:mobile_get(?LOGIN_OAUTH, Cookies, Headers ++ [
    {"X-Requested-With", "com.valvesoftware.android.steam.community"}
  ]),
  {ok, #state{cookies = Cookies ++ Response#http_response.cookies, headers = Headers}}.

handle_call({login, Username, _Password}, _From, State) ->
  RSAPostData = [{<<"username">>, Username}],
  RSAResponse = steam_http:mobile_post(?LOGIN_GET_RSA_KEY, RSAPostData, State#state.cookies, State#state.headers),
  RSABody = RSAResponse#http_response.json_body,
  case maps:get(<<"success">>, RSABody) of
    false -> throw({error, bad_rsa});
    _ -> ok
  end,


  {reply, RSAResponse, State};

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