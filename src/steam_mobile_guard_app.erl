-module(steam_mobile_guard_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  steam_mobile_guard_sup:start_link().

stop(_State) ->
  ok.
