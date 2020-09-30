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
-export([start_link/1, on/3, off/3,timer/3]).

%% gen_statem callbacks
-export([init/1, format_status/2, handle_event/4, terminate/3,
  code_change/4, callback_mode/0]).

%%-import(battery,[start_battery/1,wakeup/0,sleep/0]).
-import(battery,[]).

-define(SERVER, ?MODULE).

-record(sensor_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link(ManagerPid) ->
  gen_statem:start_link({local, ?SERVER}, ?MODULE, [ManagerPid], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init([ManagerPid]) ->
  battery:start_battery(self()),
  spawn_link(fun() -> timer(self(),random:uniform(5000)) end),
  {ok, off, {ManagerPid}}.

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



on(cast,"wake up",{})->
  battery:wakeup().

off(cast,"sleep",{})->
  battery:sleep().

%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
handle_event(_EventType, _EventContent, _StateName, State = #sensor_state{}) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _State = #sensor_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #sensor_state{}, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
on()->
  gen_statem:cast(?MODULE,"Turn On").
off()->
  gen_statem:cast(?MODULE,"Turn Off").

timer(off,SensorPid,StartTime) ->
  timer:sleep(StartTime),
  SensorPid ! "wake up",
  timer(on,SensorPid,1000);

timer(on,SensorPid,StartTime) ->
  timer:sleep(StartTime),
  SensorPid ! "Sleep",
  timer(off,SensorPid,4000).
