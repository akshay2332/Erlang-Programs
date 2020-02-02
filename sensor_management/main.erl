%%%-------------------------------------------------------------------
%%% @author akshayrane
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Nov 2019 12:04 AM
%%%-------------------------------------------------------------------
-module(main).
-author("akshayrane").

%% API
-export([start/0, setup_loop/2]).

setup_loop(MaxNoSensors, MaxNoWatchers) ->
  watcher:spawn_watchers(MaxNoSensors, MaxNoWatchers).


start() ->
  {ok, [N]} = io:fread("enter number of sensors> ", "~d"),
  if N =< 1 ->
    io:fwrite("setup: range must be at least 2~n", []);
    true ->
      Num_watchers = 1 + (N div 10),
      setup_loop(N, Num_watchers)
  end.