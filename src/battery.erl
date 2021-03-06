%%%-------------------------------------------------------------------
%%% @author sharondad
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Sep 2020 15:28
%%%-------------------------------------------------------------------
-module(battery).
-author("sharondad").

-behaviour(gen_statem).

%% API
-export([start_battery/1,wakeup/3,sleep/3,wakeup/1,sleep/1,die/3,stop/1]).

%% gen_statem callbacks
-export([init/1, format_status/2, handle_event/4, terminate/3,
  code_change/4, callback_mode/0]).

-import(sensor,[]).

-define(SERVER, ?MODULE).

-record(battery_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%% This function input is the pid of the battery sensor and
%% the output is the battery pid
start_battery(SensorPid) ->
%%  gen_statem:start_link({local, ?SERVER}, ?MODULE, [SensorPid], []).
  {ok ,Pid} = gen_statem:start_link( ?MODULE, [SensorPid], []),
  Pid.

%% when sensor turn_on -> call this func to change battery state to wakeup_state
wakeup(PID)->
  gen_statem:call(PID,"Wake Up").

%% when sensor turn_off -> call this func to change battery state to sleep_state
sleep(PID)->
  gen_statem:cast(PID,"Sleep").

%% when sensor get "no battery" MSG -> call this func to terminate the battery
stop(PID)-> gen_statem:stop(PID).


%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================
%TODO : DOC TO CALLBACK FUNC
%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init([SensorPid]) ->
  {ok, wakeup , {100,erlang:system_time(),SensorPid}}.

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

%% @private
%% @doc There should be one instance of this function for each possible
%% state name.  If callback_mode is state_functions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.

%%Sensor is wake up - if energy>0 -> battery on,
%% wait here until he goes to sleep
%% reply the sensor how much energy percent
wakeup({call,From},"Wake Up", {Energy,_Time,SensorPid}) when Energy>0 ->
%%  io:format("Energy: ~p ~n", [Energy]),
  {next_state,sleep,{Energy,erlang:system_time(),SensorPid},{reply,From,Energy}};

%%Sensor is wake up - if energy =< 0 -> battery dead, reply the sensor how much energy percent (to terminate)
wakeup({call,From},"Wake Up", {Energy,_Time,_SensorPid}) ->
  io:format("Battery: dead~n"),
%  sensor:transfer(SensorPid,"Battery dead"),
  %SensorPid ! "Battery dead",
  {next_state,die,[],{reply,From,Energy}};

wakeup(_EventType,_Other,{Energy,Time,SensorPid})->
  {next_state,wakeup,{Energy,Time,SensorPid}}.

%% No battery - die and wait here for terminate
die(_EventType,_,[])->
  {next_state,die,[]}.

%%Sensor is sleeping -> calculate how much battery and
%% stay here until he wake up
sleep(_EventType,"Sleep",{Energy,Time,SensorPid})->
  Minus =  erlang:system_time()-Time,
  NewMinus = Minus/1000000000,
  {next_state,wakeup,{Energy-NewMinus,erlang:system_time(),SensorPid}};

sleep(_EventType,_Other,{Energy,Time,SensorPid})->
  {next_state,sleep,{Energy,Time,SensorPid}}.


%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
handle_event(_EventType, _EventContent, _StateName, State = #battery_state{}) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _State ) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State , _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
