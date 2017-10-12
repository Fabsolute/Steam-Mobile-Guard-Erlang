-module(steam_cookie).

-export([
  start_link/0,
  start/0
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

  NewBot = case proplists:is_defined(Name,Bot#bot_state.cookies) of
             false->
               [proplists:property(Name)
           end,

  {reply, ok, State#state{cookies = NewCookies}};

handle_call({add_cookie, Name, Value}, _From, State = #state{cookies = Cookies}) ->
  NewCookies = case proplists:is_defined(Name, Cookies) of
                 false -> [proplists:property(Name, Value) | Cookies];
                 true -> lists:keyreplace(Name, 1, Cookies, proplists:property(Name, Value))
               end,
  {reply, ok, State#state{cookies = NewCookies}}.

handle_cast(Request, State) ->
  erlang:error(not_implemented).

handle_info(Info, State) ->
  erlang:error(not_implemented).

terminate(Reason, State) ->
  erlang:error(not_implemented).

code_change(OldVsn, State, Extra) ->
  erlang:error(not_implemented).