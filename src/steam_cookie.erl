-module(steam_cookie).

-export([
  start_link/0,
  start/0,
  add_cookie/2,
  add_cookie/3,
  add_header/2,
  add_header/3,
  get_cookies/1,
  get_cookies/0,
  get_headers/1,
  get_headers/0,
  add_cookies/1,
  add_headers/1,
  add_headers/2,
  add_cookies/2
]).

-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-behavior(gen_server).

-record(state, {
  cookies = [],
  headers = [],
  bots = []
}).

-record(bot_state, {
  username = null,
  cookies = [],
  headers = []
}).

%% API
start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_cookie(Name, Value) ->
  gen_server:call(?MODULE, {add_cookie, Name, Value}).

add_cookies([]) ->
  ok;

add_cookies([{Name, Value} | Cookies]) ->
  add_cookie(Name, Value),
  add_cookies(Cookies).

add_cookie(BotName, Name, Value) ->
  gen_server:call(?MODULE, {add_cookie, BotName, Name, Value}).

add_cookies(_BotName, []) ->
  ok;

add_cookies(BotName, [{Name, Value} | Cookies]) ->
  add_cookie(BotName, Name, Value),
  add_cookies(Cookies).

add_header(Name, Value) ->
  gen_server:call(?MODULE, {add_header, Name, Value}).

add_headers([]) ->
  ok;

add_headers([{Name, Value} | Headers]) ->
  add_header(Name, Value),
  add_headers(Headers).

add_header(BotName, Name, Value) ->
  gen_server:call(?MODULE, {add_header, BotName, Name, Value}).

add_headers(_BotName, []) ->
  ok;

add_headers(BotName, [{Name, Value} | Headers]) ->
  add_header(BotName, Name, Value),
  add_headers(Headers).

get_cookies(BotName) ->
  gen_server:call(?MODULE, {get_cookies, BotName}).

get_cookies() ->
  gen_server:call(?MODULE, get_cookies).

get_headers(BotName) ->
  gen_server:call(?MODULE, {get_headers, BotName}).

get_headers() ->
  gen_server:call(?MODULE, get_headers).

%% gen_server

init([]) ->
  {ok, #state{}}.

handle_call({add_cookie, BotName, Name, Value}, _From, State = #state{bots = Bots}) ->
  Bot = case proplists:is_defined(BotName, Bots) of
          false ->
            #bot_state{username = BotName};
          true ->
            proplists:get_value(BotName, Bots)
        end,

  NewBot = case proplists:is_defined(Name, Bot#bot_state.cookies) of
             false ->
               Bot#bot_state{cookies = [proplists:property(Name, Value) | Bot#bot_state.cookies]};
             true ->
               Bot#bot_state{cookies = lists:keyreplace(Name, 1, Bot#bot_state.cookies, proplists:property(Name, Value))}
           end,
  NewBots = case proplists:is_defined(BotName, Bots) of
              false ->
                [NewBot | Bots];
              true ->
                lists:keyreplace(BotName, 1, Bots, NewBot)
            end,

  {reply, ok, State#state{bots = NewBots}};

handle_call({add_header, BotName, Name, Value}, _From, State = #state{bots = Bots}) ->
  Bot = case proplists:is_defined(BotName, Bots) of
          false ->
            #bot_state{username = BotName};
          true ->
            proplists:get_value(BotName, Bots)
        end,

  NewBot = case proplists:is_defined(Name, Bot#bot_state.headers) of
             false ->
               Bot#bot_state{headers = [proplists:property(Name, Value) | Bot#bot_state.headers]};
             true ->
               Bot#bot_state{headers = lists:keyreplace(Name, 1, Bot#bot_state.headers, proplists:property(Name, Value))}
           end,
  NewBots = case proplists:is_defined(BotName, Bots) of
              false ->
                [NewBot | Bots];
              true ->
                lists:keyreplace(BotName, 1, Bots, NewBot)
            end,

  {reply, ok, State#state{bots = NewBots}};

handle_call({add_cookie, Name, Value}, _From, State = #state{cookies = Cookies}) ->
  NewCookies = case proplists:is_defined(Name, Cookies) of
                 false ->
                   [proplists:property(Name, Value) | Cookies];
                 true ->
                   lists:keyreplace(Name, 1, Cookies, proplists:property(Name, Value))
               end,
  {reply, ok, State#state{cookies = NewCookies}};

handle_call({add_header, Name, Value}, _From, State = #state{headers = Headers}) ->
  NewHeaders = case proplists:is_defined(Name, Headers) of
                 false ->
                   [proplists:property(Name, Value) | Headers];
                 true ->
                   lists:keyreplace(Name, 1, Headers, proplists:property(Name, Value))
               end,
  {reply, ok, State#state{headers = NewHeaders}};

handle_call({get_cookies, BotName}, _From, State = #state{bots = Bots, cookies = Cookies}) ->
  Cookie = case proplists:is_defined(BotName, Bots) of
             false ->
               Cookies;
             true ->
               BotState = proplists:get_value(BotName, Bots),
               BotState#bot_state.cookies ++ Cookies
           end,
  {reply, Cookie, State};

handle_call(get_cookies, _From, State = #state{cookies = Cookies}) ->
  {reply, Cookies, State};

handle_call({get_headers, BotName}, _From, State = #state{bots = Bots, headers = Headers}) ->
  Header = case proplists:is_defined(BotName, Bots) of
             false ->
               Headers;
             true ->
               BotState = proplists:get_value(BotName, Bots),
               BotState#bot_state.headers ++ Headers
           end,
  {reply, Header, State};

handle_call(get_headers, _From, State = #state{headers = Headers}) ->
  {reply, Headers, State}.


handle_cast(_Request, State) ->
  {noreply, State}.
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
