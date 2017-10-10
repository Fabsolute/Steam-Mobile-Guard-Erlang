-module(steam_time_aligner).
-include("../include/steam_urls.hrl").
-include("../include/http_response.hrl").

-behavior(gen_server).

-export([
  start/0,
  start_link/0,
  get_steam_time/0,
  get_local_time/0
]).

-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-record(state, {
  is_time_aligned = false,
  aligned_diff = 0
}).

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_time_diff() ->
  gen_server:call(?MODULE, time_diff).

get_steam_time() ->
  Time = get_local_time(),
  AlignedDiff = get_time_diff(),
  AlignedDiff + Time.

get_time_diff_from_server() ->
  ServerTime = get_server_time(),
  LocalTime = get_local_time(),
  ServerTime - LocalTime.

get_server_time() ->
  Url = ?TWO_FACTOR_TIME_QUERY,

  SteamResponse = steam_http:post(Url ++ "?steam_id=0", <<>>, [], []),
  Response = maps:get(<<"response">>, SteamResponse#http_response.json_body),
  ServerTimeString = maps:get(<<"server_time">>, Response),
  ServerTime = list_to_integer(binary_to_list(ServerTimeString)),
  ServerTime.

get_local_time() -> erlang:system_time(1).


init([]) ->
  {ok, #state{}}.

handle_call(time_diff, _From, #state{is_time_aligned = IsTimeAligned, aligned_diff = AlignedDiff}) ->
  ResponseTimeDiff = case IsTimeAligned of
                       false -> get_time_diff_from_server();
                       _ -> AlignedDiff
                     end,
  State = #state{is_time_aligned = true, aligned_diff = ResponseTimeDiff},
  {reply, ResponseTimeDiff, State}.

handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
