%%%-------------------------------------------------------------------
%%% @author fabsolutely
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Dec 2016 07:20
%%%-------------------------------------------------------------------
-module(steam_guard_code_time_aligner).
-author("fabsolutely").
-include("steam_guard_code_constants.hrl").


%% API
-export([start/0, time_aligner_keeper/2, get_time_diff/0, get_steam_time/0, get_time/0]).

start() ->
  Pid = spawn_link(?MODULE, time_aligner_keeper, [0, false]),
  register(steam_guard_code_time_aligner_keeper_pid, Pid).

time_aligner_keeper(TimeDiff, IsTimeAligned) ->
  receive
    {get_time, Pid} -> case IsTimeAligned of
                         true -> Pid ! {time_response, TimeDiff},
                           time_aligner_keeper(TimeDiff, IsTimeAligned);
                         _ -> ServerTimeDiff = get_time_diff_from_server(),
                           Pid ! {time_response, ServerTimeDiff},
                           time_aligner_keeper(ServerTimeDiff, true)
                       end;
    _ -> time_aligner_keeper(TimeDiff, IsTimeAligned)
  end.

get_steam_time() ->
  Diff = get_time_diff(),
  LocalTime = get_time(),
  LocalTime + Diff.


get_time_diff() ->
  steam_guard_code_time_aligner_keeper_pid ! {get_time, self()},
  receive
    {time_response, TimeDiff} -> TimeDiff
  end.

get_time_diff_from_server() ->
  ServerTime = get_server_time(),
  LocalTime = get_time(),
  ServerTime - LocalTime.

get_server_time() ->
  Url = ?TWO_FACTOR_TIME_QUERY,
  Response = httpc:request(post, {Url, [], "application/json", "steam_id=0"}, [], []),

  ResponseObject = case Response of
                     {ok, Result} ->
                       case Result of
                         {_Status, _Headers, Body} -> jiffy:decode(Body, [return_maps]);
                         {_Status, Body} -> jiffy:decode(Body, [return_maps])
                       end
                   end,
  SteamResponse = maps:get(<<"response">>, ResponseObject),
  ServerTimeString = maps:get(<<"server_time">>, SteamResponse),
  ServerTime = list_to_integer(binary_to_list(ServerTimeString)),
  ServerTime.

get_time() ->
  erlang:system_time(1).