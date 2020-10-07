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
-export([start_sensor/3, on/3, off/3,timer/3,timer/2, on/1, off/1,transfer/3, transfer/2,startSensor/3,dead/3,tempFunc/2,windFunc/2]).

%% gen_statem callbacks
-export([init/1, format_status/2, handle_event/4, terminate/3,
  code_change/4, callback_mode/0,stop/1]).

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
start_sensor(ManagerPid,X,Y) ->
%%  {ok ,Pid} = gen_statem:start_link(?MODULE, [ManagerPid,X,Y], []),
  {ok ,Pid} = gen_statem:start(?MODULE, [ManagerPid,X,Y], []),
  Pid.
%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init([ManagerPid,X,Y]) ->
  BatPid = battery:start_battery(self()),
  {ok, startSensor, {BatPid, ManagerPid,X,Y}}.

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


startSensor(_EventType,"Turn On",{BatPid, ManagerPid,X,Y}) ->
%%  spawn_link(fun() -> timer(on, self(),rand:uniform(5000)) end),
  SelfPid = self(),
  if
    {X,Y} == {0,0} -> spawn(fun() -> timer(on, SelfPid) end);
    true -> spawn(fun() -> timer(on, SelfPid,rand:uniform(10000)) end)
  end,

  {next_state, transfer, {BatPid, ManagerPid,X,Y}}.

on(_EventType,"wake up", {BatPid, ManagerPid,X,Y})->
  battery:wakeup(BatPid),
  slaveManager:send(ManagerPid,{"I woke up", {X,Y}}),
  slaveManager:send(ManagerPid,{{tempFunc(X,Y),windFunc(X,Y)}, {X,Y}, {X,Y}}),
  {next_state,transfer,  {BatPid, ManagerPid,X,Y}};

on(_EventType,_other, {BatPid, ManagerPid,X,Y})->
  {next_state,on,  {BatPid, ManagerPid,X,Y}}.

transfer(_EventType,{{Temp,Wind}, SenderLocation} , {BatPid, ManagerPid,X,Y})->
  if
    {X,Y} == {0,0} -> ok ; %TODO update data table
%%    {X,Y} == {0,0} -> io:format("{0,0} entrey: recived from ~p messege ~p~n",[SenderLocation,{Temp,Wind}]) ;
    true ->
      Val = put(SenderLocation,{SenderLocation,Temp,Wind}),
      if
           Val == undefined -> slaveManager:send(ManagerPid,{{Temp,Wind}, SenderLocation, {X,Y}});
         true -> ok
      end
  end,
  {next_state,transfer,  {BatPid, ManagerPid,X,Y}};

transfer(_EventType,"Exit" , {BatPid, ManagerPid,X,Y})->
  {next_state,off,  {BatPid, ManagerPid,X,Y}};

transfer(_EventType,"Battery dead" , {BatPid, ManagerPid,X,Y})->
  slaveManager:send(ManagerPid,{"Battery dead", {X,Y}}),
  battery:stop(BatPid),
  {next_state,dead,{}};

transfer(_EventType,_FireAlert , {BatPid, ManagerPid,X,Y})->
  io:format("Fire or Unknown, Run!!!!!!!!!!!!!"),
  {next_state,transfer,  {BatPid, ManagerPid,X,Y}}.



off(_EventType,"sleep",{BatPid, ManagerPid,X,Y})->
  slaveManager:send(ManagerPid,{"Im going to sleep", {X,Y}}),
%%  ManagerPid ! {"Im going to sleep", {X,Y}},
  erase(),
  c:flush(),
  battery:sleep(BatPid),
  {next_state, on, {BatPid, ManagerPid,X,Y}}.


dead(_EventType, _Message, {}) ->
  {next_state, dead,{}}.
%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
handle_event(_EventType, _EventContent, _StateName, State) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

%% @private
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
on(Pid)->
  gen_statem:cast(Pid,"Turn On").
off(Pid)->
  gen_statem:stop(Pid).
transfer(Pid, Message)->
  gen_statem:cast(Pid,Message).
stop(PID)-> gen_statem:stop(PID).


timer(off,SensorPid,StartTime) ->
  timer:sleep(StartTime),
  SensorPid ! "wake up",
  timer(on,SensorPid,2000);

timer(on,SensorPid,StartTime) ->
  timer:sleep(StartTime),
  SensorPid ! "Exit",
  SensorPid ! "sleep",
  timer(off,SensorPid,8000).

timer(on,SensorPid) ->
  timer:sleep(100000000),
  timer(on,SensorPid).


tempFunc(X,Y) ->
  OneTime = erlang:system_time()/9,
  IntTime = round(OneTime),
  TwoTime = IntTime rem 100,
  NewTemp = abs(math:sin((X+Y+0.1)*TwoTime)*40),
  NewTemp.

windFunc(X,Y) ->
  OneWind = erlang:system_time()/9,
  IntWind = round(OneWind),
  TwoWind = IntWind rem 100,
  NewWind = abs(math:sin((X+Y+0.1)*TwoWind)*30),
  NewWind.

