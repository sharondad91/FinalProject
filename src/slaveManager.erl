%%%-------------------------------------------------------------------
%%% @author sharondad
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(slaveManager).

-behaviour(gen_server).

-export([start_server/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3,test/0]).

-import(sensor,[]).
-define(SERVER, ?MODULE).

-record(slaveMaster_state, {}).
%-record(sensorData, {cord,pid,status,neighborList}).
-record(sensorData, {x,y}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_server(Name,TableName, {StartX,EndX},{StartY,EndY}) ->
  gen_server:start_link({global, Name}, ?MODULE, [TableName, {StartX,EndX},{StartY,EndY}], []).

init([TableName, {StartX,EndX},{StartY,EndY}]) ->
  MyPid = self(),
  mnesia:create_schema(node()),
  mnesia:start(),
  OK = mnesia:create_table(TableName,[{access_mode, read_write}, {type, set}, {record_name, sensorData}, {attributes, record_info(fields, sensorData)}]),
%%  R = #sensorData{cord =3, pid = 0.87, status = on,neighborList = []},
  R = #sensorData{x ={3,4}, y ={ 0.87, on, []}},
  P = #sensorData{x ={2,6}, y ={ 0.87, off, [{3,4}]}},
  mnesia:dirty_write(TableName,R),
  mnesia:dirty_write(TableName,P),
  Q = mnesia:dirty_read(TableName,{3,4}),
  W = mnesia:dirty_read(TableName,{2,6}),
  io:format("dirty ~p~n",[Q]),
  io:format("dirty ~p~n",[W]),
  {ok, OK}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
test() ->
  Pid = self(),
  SensorPid = sensor:start_sensor(Pid,1234,1212),
  sensor:on(SensorPid),
  block().

block() ->
  receive
    Temp->Temp,
      io:format("Mes: ~p ~n",[Temp])
  end,
  block().