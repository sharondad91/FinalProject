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
  code_change/3,test/0,transferMessages/1]).

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
%%  R = #sensorData{x ={3,4}, y ={ 0.87, on, []}},
%%  P = #sensorData{x ={2,6}, y ={ 0.87, off, [{3,4}]}},
%%  mnesia:dirty_write(TableName,R),
%%  mnesia:dirty_write(TableName,P),
%%  Q = mnesia:dirty_read(TableName,{3,4}),
%%  W = mnesia:dirty_read(TableName,{2,6}),
%%  io:format("dirty ~p~n",[Q]),
%%  io:format("dirty ~p~n",[W]),
  createTableRow(TableName, MyPid,StartX,StartY,EndX,EndY),
  transferMessages(TableName),
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

createTableRow(TableName, ManagerPid,X,Y,EndX,EndY) when X =< EndX  ->
  createTableCol(TableName, ManagerPid,X,Y,EndY),
  createTableRow(TableName, ManagerPid,X+1,Y,EndX,EndY);

createTableRow(_TableName, _ManagerPid,_X,_Y,_EndX, _EndY) -> ok.

createTableCol(TableName, ManagerPid,X,Y,EndY) when Y=< EndY ->
  if
    X>0  ->
      if
        Y>0 -> List = [{X-1,Y}, {X,Y-1},{X-1,Y-1}];
        true -> List = List = [{X-1,0}]
        end;
    true ->
      if
        Y>0 -> List = [{0,Y-1}];
        true -> List = []
        end
  end,
  TempSensorPid = sensor:start_sensor(ManagerPid,X,Y),
  R = #sensorData{x ={X,Y}, y ={ TempSensorPid, true, List}},
  mnesia:dirty_write(TableName,R),
  sensor:on(TempSensorPid),
%%  W = mnesia:dirty_read(TableName,{X,Y}),
%%  io:format("Sensor created: ~p~n",[W]),
  createTableCol(TableName, ManagerPid,X,Y+1,EndY);

createTableCol(_TableName, _ManagerPid,_X,_Y,_EndY) -> ok.

transferMessages(TableName) ->
  receive
    {{Temp,Wind},SenderLocation,TransferLocation} -> funcTransfer(TableName,{Temp,Wind},SenderLocation,TransferLocation);
    {"I woke up", {X,Y}} -> switchOn(TableName,X,Y);
    {"Im going to sleep", {X,Y}} -> switchOff(TableName,X,Y);
     _Other -> io:format("No Match~n")
  end,
  transferMessages(TableName).

switchOn(TableName,X,Y) ->
  [{_NameTable,Location,{Pid,Status, NeighborList}}] = mnesia:dirty_read(TableName, {X,Y}),
  R = #sensorData{x =Location, y ={ Pid, not Status, NeighborList}},
  mnesia:dirty_write(TableName,R),
  io:format("{~p,~p} Swiched on~n", [X,Y]).


switchOff(TableName,X,Y) ->
 [{_NameTable,Location,{Pid,Status, NeighborList}}] = mnesia:dirty_read(TableName, {X,Y}),
  R = #sensorData{x =Location, y ={ Pid, not Status, NeighborList}},
  mnesia:dirty_write(TableName,R),
  io:format("{~p,~p} Swiched off~n", [X,Y]).

funcTransfer(TableName, {Temp,Wind},SenderLocation,TransferLocation) ->
  Q = mnesia:dirty_read(TableName,TransferLocation),
  [{_NameTable,_Location,{_Pid,_Status, NeighborList}}]= Q,
  Forward = fun(Sensor) -> sendMessage(TableName, {Temp,Wind},SenderLocation,Sensor) end,
  lists:foreach(Forward,NeighborList).

sendMessage(TableName, {Temp,Wind},SenderLocation ,NextLocation) ->
  [{_NameTable,_Location,{NextPid,Status, _NeighborList}}] = mnesia:dirty_read(TableName,NextLocation),
  if
    Status == true -> NextPid ! {{Temp,Wind},SenderLocation},
      io:format("Transfer data from: ~p to ~p~n", [SenderLocation,NextLocation]);
    true -> ok
  end.
