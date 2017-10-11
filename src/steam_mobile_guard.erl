-module(steam_mobile_guard).

%% API
-export([start/0, stop/0]).


start() ->
  reloader:start(),
  steam_time_aligner:start(),
  steam_cookie:start(),
  steam_bot:start(),
  start(steam_mobile_guard).

start(Application) ->
  case application:ensure_started(Application) of
    ok -> io:format("i am ~p and i started ~n", [Application]);
    _Error -> case application:get_key(Application, applications) of
                {ok, DependentApplications} ->
                  io:format("i am ~p and i require ~p ~n", [Application, DependentApplications]),
                  [start(A) || A <- DependentApplications],
                  start(Application);
                X -> io:format("i am ~p and i failed with ~p ~n", [Application, X])
              end
  end,
  ok.

stop() -> ok.