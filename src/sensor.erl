%%%-------------------------------------------------------------------
%%% @author sharondad
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Sep 2020 15:12
%%%-------------------------------------------------------------------
-module(sensor).
-author("sharondad").

-behaviour(gen_statem).

%% API
-export([start_sensor/4,  on/1, off/1,transfer/2, stop/1]).

%% gen_statem callbacks
-export([init/1, format_status/2, handle_event/4, terminate/3,
  code_change/4, callback_mode/0,on/3, off/3,transfer/3, startSensor/3,dead/3]).


%%internalFunc
-export([timer/3,timer/4,tempFunc/3,windFunc/3]).

%%-import(battery,[start_battery/1,wakeup/0,sleep/0]).
-import(battery,[]).
-import(slaveManager,[]).

-define(SERVER, ?MODULE).

%%-record(sensor_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%% This function input is the pid of the sensor's Manager,
%% the sensor coordinates - X,Y and the Center_Coordinates (who update the table)
%% the output is the sensor pid
start_sensor(ManagerPid,X,Y,Center) ->
  {ok ,Pid} = gen_statem:start(?MODULE, [ManagerPid,X,Y,Center], []),
  Pid.

%% After all the sensors created -> the relevant slaveManager
%% turn on the sensor (with pid = Pid) by this func
on(Pid)->
  gen_statem:cast(Pid,"Turn On").

off(Pid)->
  gen_statem:stop(Pid).

%% transfer message Message to the sensor (with pid = Pid)
transfer(Pid, Message)->
  if
    Message == "Battery dead" -> io:format("no no no~n");
    true -> ok
  end,
%%  Energy
  gen_statem:cast(Pid,Message).

%% After battery's sensor (with pid = Pid) dead ->
%% his slaveManager use this func to terminate his process (Pid)
stop(PID)-> gen_statem:stop(PID).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%% Start Battery process
%% next state transfer -> waiting there to timer "Exit" msg and then sleep
init([ManagerPid,X,Y,Center]) ->
  BatPid = battery:start_battery(self()),
  {ok, startSensor, {BatPid, ManagerPid,X,Y,Center}}.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
  state_functions.

%% @private
%% @doc Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
format_status(_Opt, [_PDict, _StateName, _State]) ->
  Status = some_term,
  Status.

%% When all the sensors are started -> slave send "turn on" that's means sensor start his job
%% start timers to sleep and wake up msg
startSensor(_EventType,"Turn On",{BatPid, ManagerPid,X,Y,Center}) ->
  SelfPid = self(),
  if
  %%if the sensor is the Stationary position -> the timer is only to update the state but never go to sleep
    {X,Y} == {Center,Center} ->
      spawn(fun() -> timer(on, SelfPid,X,Y) end);
  %%else, the sensor is normal position -> the timer is for sleep and wake up , with random start value
    true ->
      spawn(fun() -> timer(on, SelfPid,rand:uniform(4000)) end)
  end,

  {next_state, transfer, {BatPid, ManagerPid,X,Y,Center}}.

%%when timer sais "wake up" the sensor is on state on
%% update battery to turn on and get the energy
%% if energy <= 0 -> update his slave-manager that he is dead and the die
%% else - check what is the temp and wind and update the slave-manager to send to his neighbors
%%        then move to transfer to transfer his neighbors msgs
on(_EventType,"wake up", {BatPid, ManagerPid,X,Y,Center})->
  Energy = battery:wakeup(BatPid),
  slaveManager:send(ManagerPid,{"I woke up", {X,Y},Energy}),
  if
    Energy =<0 ->
      io:format("Sensor: battery dead ~p~n",[{X,Y}]),
      slaveManager:send(ManagerPid,{"Battery dead", {X,Y}}),
      battery:stop(BatPid),
      {next_state,dead,{}};
    true ->
      Temp = tempFunc(X,Y,Center),
      Wind = windFunc(X,Y,Center),
      slaveManager:send(ManagerPid,{{Temp,Wind,0,Energy}, {X,Y}, {X,Y}}),
      {next_state,transfer,  {BatPid, ManagerPid,X,Y,Center}}
  end;

%%if sensor get an fire msg
%% he sent it to his slave-manager , stop the battery and then die
on(_EventType,"Fire!!!", {BatPid, ManagerPid,X,Y,_Center})->
  battery:stop(BatPid),
  slaveManager:send(ManagerPid,{"Sensor on Fire!!!", {X,Y}}),
  {next_state,dead,  {}};

on(_EventType,_Other, {BatPid, ManagerPid,X,Y,Center})->
  {next_state,on,  {BatPid, ManagerPid,X,Y,Center}}.


%%transfer msg {Temp,Wind,Count, Energy} from SenderLocation
%% if the sensor is the stationary position -> update the master to update the dataBase
%% else, update his slave-manager to update the neighbors
transfer(_EventType,{{Temp,Wind,Count,Energy}, SenderLocation} , {BatPid, ManagerPid,X,Y,Center})->
  if
    {X,Y} == {Center,Center} ->
      masterManager:send({"Update Data Table",SenderLocation, {Temp,Wind,Count+1,Energy}});
    true ->
      Val = put(SenderLocation,{SenderLocation,Temp,Wind}),
      if
        %check if send this msg in this cycle-time -> send only if not sent before
           Val == undefined ->
             slaveManager:send(ManagerPid,{{Temp,Wind,Count+1,Energy}, SenderLocation, {X,Y}});
         true ->
           ok
      end
  end,
  {next_state,transfer,  {BatPid, ManagerPid,X,Y,Center}};

%%if exit-> go to off
transfer(_EventType,"Exit" , {BatPid, ManagerPid,X,Y,Center})->
  {next_state,off,  {BatPid, ManagerPid,X,Y,Center}};

%%if slave get an fire msg
%% he sent it to his slave (manager) , stop the battery and then die
transfer(_EventType,"Fire!!!" , {BatPid, ManagerPid,X,Y,_Center})->
  battery:stop(BatPid),
  slaveManager:send(ManagerPid,{"Sensor on Fire!!!", {X,Y}}),
  {next_state,dead,  {}};


transfer(_EventType,_Other , {BatPid, ManagerPid,X,Y,Center})->
  {next_state,transfer,  {BatPid, ManagerPid,X,Y,Center}}.


%%when off -> update the slave-manager, earse the msg from this cycle
%% turn off the battery
%% next state is on
off(_EventType,"sleep",{BatPid, ManagerPid,X,Y,Center})->
  slaveManager:send(ManagerPid,{"Im going to sleep", {X,Y}}),
  erase(),
  battery:sleep(BatPid),
  receive
    "Fire!!!"->
      battery:stop(BatPid),
      slaveManager:send(ManagerPid,{"Sensor on Fire!!!", {X,Y}}),
      {next_state, dead, {}}
  after 100 ->
    {next_state, on, {BatPid, ManagerPid,X,Y,Center}}
  end;

%%if slave get an fire msg
%% he sent it to his slave (manager) , stop the battery and then die
off(_EventType,"Fire!!!",{BatPid, ManagerPid,X,Y,_Center})->
  battery:stop(BatPid),
  slaveManager:send(ManagerPid,{"Sensor on Fire!!!", {X,Y}}),
  {next_state,dead,  {}};

off(_EventType,_Other,{BatPid, ManagerPid,X,Y,Center})->
  {next_state, off, {BatPid, ManagerPid,X,Y,Center}}.

%% battery over or sensor fired -> dead and wait here until terminate by the slave
dead(_EventType, _Message, {}) ->
  {next_state, dead,{}}.
%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
handle_event(_EventType, _EventContent, _StateName, State) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

%% @privatema
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _State) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%change status from off to on - off timer for StartTime microsec,
%% set on timer for 1600 micro sec
timer(off,SensorPid,StartTime) ->
  timer:sleep(StartTime),
  SensorPid ! "wake up",
  timer(on,SensorPid,1600);

%%change status from on to off - on timer for StartTime microsec
%% set off timer for 2400 micro sec
timer(on,SensorPid,StartTime) ->
  timer:sleep(StartTime),
  SensorPid ! "Exit",
  SensorPid ! "sleep",
  timer(off,SensorPid,2400).

%% timer for stationary position is only for update sensor data
timer(on,SensorPid,X,Y) ->
  timer:sleep(5000),
  masterManager:send({"Update Data Table", {X,Y},{tempFunc(X,Y,5),windFunc(X,Y,5),0,100}}),
  timer(on,SensorPid,X,Y).


%%temperature function to sensor at position X,Y
tempFunc(X,Y,_Center) ->
%%  Mes = (10/Center),
  OneTime = erlang:system_time()/9,
  IntTime = round(OneTime),
  TwoTime = IntTime rem 100,
  NewTemp = abs(math:sin((X+Y+0.1)*TwoTime))*rand:uniform(40),
%%  NewTemp = abs((math:sin((X+Y+0.1)*TwoTime))*(X+Y)*Mes),
  NewTemp.

%%wind function to sensor at position X,Y
windFunc(X,Y,Center) ->
  Mes = (10/Center),
  OneWind = erlang:system_time()/9,
  IntWind = round(OneWind),
  TwoWind = IntWind rem 100,
  NewWind = abs(math:sin((X+Y+0.1)*TwoWind)*(X+Y)*Mes),
  NewWind.

