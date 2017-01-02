%%%-------------------------------------------------------------------
%%% @author fabsolutely
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Jan 2017 06:31
%%%-------------------------------------------------------------------
-module(steam_guard_code_generator_app).
-author("fabsolutely").
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  steam_guard_code_generator_sup:start_link().

stop(_State) ->
  ok.