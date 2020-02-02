%%%-------------------------------------------------------------------
%%% @author akshayrane
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Nov 2019 12:04 AM
%%%-------------------------------------------------------------------
-module(sensor).
-author("akshayrane").

%% API
-export([sensor/2]).

sensor(Id, WatcherPid) ->
  io:fwrite("sensor : start: ~p~n", [[Id, WatcherPid]]),
  Measurement = rand:uniform(11),
  sensor_messages(Id, WatcherPid, Measurement).

sensor_messages(Id, WatcherPid, Measurement) ->

  if (Measurement >= 1) and (Measurement < 11) ->
    WatcherPid ! {Id, Measurement};
    true ->
      io:fwrite("watcher: SensorId [~p] | Measurement (~p)~n", [Id, Measurement]),
      exit(anomalous_reading)
  end,
  Sleep_time = rand:uniform(10000),
  timer:sleep(Sleep_time),
  NewMeasurement = rand:uniform(11),
  sensor_messages(Id, WatcherPid, NewMeasurement).
