%%%-------------------------------------------------------------------
%%% @author akshayrane
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Nov 2019 12:04 AM
%%%-------------------------------------------------------------------
-module(watcher).
-author("akshayrane").

%% API
-export([spawn_watchers/2, watcher/2]).

spawn_sensor(SensorId) ->
  {SensorPid, _} = spawn_monitor(sensor, sensor, [SensorId, self()]),
  io:fwrite("watcher : start: ~p~n", [[SensorPid, self()]]),
  SensorPid.

spawn_sensors_recursively(SensorId, NumberOfSensors, SensorList) when NumberOfSensors == 0 ->
  SensorList;
spawn_sensors_recursively(SensorId, NumberOfSensors, SensorList) when NumberOfSensors > 0 ->
  SensorPid = spawn_sensor(SensorId),
  NewSensorList = lists:append([{SensorPid, SensorId}], SensorList),
  spawn_sensors_recursively(SensorId + 1, NumberOfSensors - 1, NewSensorList).

watcher(StartSensorId, NumberOfSensors) ->
  Sensors = spawn_sensors_recursively(StartSensorId, NumberOfSensors, []),
  io:fwrite("watcher: Sensor List ~p~n", [Sensors]),
  listen_sensors(Sensors).


listen_sensors(Sensors) ->
  receive
    {'DOWN', _, process, SensorPid, Reason} ->
      {_, SensorId} = lists:keyfind(SensorPid, 1, Sensors),
      io:fwrite("watcher: Sensor Down Id [~p] | Reason (~s)~n", [SensorId, Reason]),
      Restart_Pid = spawn_sensor(SensorId),
      New_Sensor_List = lists:keyreplace(SensorId, 2, Sensors, {Restart_Pid, SensorId}),
      io:fwrite("watcher: Sensor List after restart ~p~n", [New_Sensor_List]),
      listen_sensors(New_Sensor_List);
    {SensorId, Measurement} ->
      io:fwrite("watcher: SensorId [~p] | Measurement (~p)~n", [SensorId, Measurement]),
      listen_sensors(Sensors)
  end.

spawn_watcher_process(StartIndex, MaxNoSensors, CountOfSensors) when (MaxNoSensors - StartIndex) > CountOfSensors ->
  spawn(?MODULE, watcher, [StartIndex, CountOfSensors]);
spawn_watcher_process(StartIndex, MaxNoSensors, CountOfSensors) ->
  spawn(?MODULE, watcher, [StartIndex, MaxNoSensors - StartIndex]).

spawn_watchers_recursively(StartIndex, MaxNoWatchers, CountOfSensors, MaxNoSensors) when MaxNoWatchers > 0 ->
  spawn_watcher_process(StartIndex, MaxNoSensors, CountOfSensors),
  spawn_watchers_recursively(StartIndex + CountOfSensors, (MaxNoWatchers - 1), CountOfSensors, MaxNoSensors);
spawn_watchers_recursively(StartIndex, MaxNoWatchers, CountOfSensors, MaxNoSensors) ->
  todo.

spawn_watchers(MaxNoSensors, MaxNoWatchers) ->
  spawn_watchers_recursively(0, MaxNoWatchers, 10, MaxNoSensors).
