%%%-------------------------------------------------------------------
%%% @author sharondad
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(slaveManager).

-behaviour(gen_server).

-export([start_server/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3,send/2]).

-import(sensor,[]).
-define(SERVER, ?MODULE).

%%-record(slaveMaster_state, {}).
-record(sensorData, {x,y}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

%%start_server(Name,TableName, {StartX,EndX},{StartY,EndY}) ->
%%  gen_server:start_link({global, Name}, ?MODULE, [TableName, {StartX,EndX},{StartY,EndY}], []).
start_server(Name,TableName, Index) ->
  gen_server:start_link({global, Name}, ?MODULE, [TableName, {0,Index},{0,Index}], []).


init([TableName, {StartX,EndX},{StartY,EndY}]) ->
  MyPid = self(),
  mnesia:create_schema(node()),
  mnesia:start(),
  mnesia:create_table(TableName,[{access_mode, read_write}, {type, set}, {record_name, sensorData}, {attributes, record_info(fields, sensorData)}]),
  createTableRow(TableName,3, MyPid,StartX,StartY,{StartX,StartY,EndX,EndY}),
  {ok, TableName}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({{Temp,Wind},SenderLocation,TransferLocation}, TableName) ->
  funcTransfer(TableName,{Temp,Wind},SenderLocation,TransferLocation),
  {noreply, TableName};

handle_cast({"I woke up", {X,Y}}, TableName) ->
  switchOn(TableName,X,Y),
  {noreply, TableName};

handle_cast({"Im going to sleep", {X,Y}}, TableName) ->
  switchOff(TableName,X,Y),
  {noreply, TableName};

handle_cast({"Battery dead", {X,Y}}, TableName) ->
%%  TODO send to the MasterManger
  [{_NameTable,_Location,{SensorPid,_Status, _NeighborList}}] = mnesia:dirty_read(TableName, {X,Y}),
  sensor:stop(SensorPid),
  {noreply, TableName};

handle_cast(_Other, TableName) ->
  {noreply, TableName}.


handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

createTableRow(TableName,Rad, ManagerPid,X,Y,{StartX,StartY,EndX,EndY}) when X =< EndX  ->
  createTableCol(TableName,Rad, ManagerPid,X,Y,{StartX,StartY,EndX,EndY}),
  createTableRow(TableName,Rad, ManagerPid,X+1,Y,{StartX,StartY,EndX,EndY});

createTableRow(_TableName,_Rad, _ManagerPid,_X,_Y,_Boundaries) -> ok.

createTableCol(TableName,Rad, ManagerPid,X,Y,{StartX,StartY,EndX,EndY}) when Y=< EndY ->
  TempSensorPid = sensor:start_sensor(ManagerPid,X,Y),
  R = #sensorData{x ={X,Y}, y ={ TempSensorPid, true, []}},
  mnesia:dirty_write(TableName,R),
  createNeighbors(TableName,Rad,X,Y),
  sensor:on(TempSensorPid),
  createTableCol(TableName,Rad, ManagerPid,X,Y+1,{StartX,StartY,EndX,EndY});

createTableCol(_TableName,_Rad, _ManagerPid,_X,_Y,_Boundaries) -> ok.

createNeighbors(TableName,Rad,X,Y) when Rad>=1 ->
%%  io:format("create neighbors for {~p,~p}~n",[X,Y]),
  create(TableName,Rad,right,X,Y,X+1,Y),
%%  io:format("create Right neighbors~n"),
  create(TableName,Rad,up,X,Y,X,Y-1),
%%  io:format("create UP neighbors~n"),
  create(TableName,Rad,left,X,Y,X-1,Y),
%%  io:format("create Left neighbors~n"),
  create(TableName,Rad,down,X,Y,X,Y+1),
  Q=mnesia:dirty_read(TableName, {X,Y}),
  io:format("{~p,~p} data is : ~p~n",[X,Y,Q]).





create(TableName,Rad,From,XPoint,YPoint,Xnei,Ynei)->
  [{_NameTable,_Location,{_Pid,_Status, NeighborList}}] = mnesia:dirty_read(TableName, {XPoint,YPoint}),
  Exist = lists:member({Xnei,Ynei},NeighborList),
  if
    Exist == true -> [];
    true ->XPow = math:pow(XPoint-Xnei,2),
      YPow = math:pow(YPoint-Ynei,2),
      RPow = math:pow(Rad,2),
      if
        (XPow+YPow)=< RPow ->
          addNeighbor(TableName,XPoint,YPoint,Xnei,Ynei),
%%          io:format("{~p,~p}~n",[Xnei,Ynei]),
          case From of
            left ->
              create(TableName,Rad,left,XPoint,YPoint,Xnei-1,Ynei),
              create(TableName,Rad,up,XPoint,YPoint,Xnei,Ynei-1),
              create(TableName,Rad,down,XPoint,YPoint,Xnei,Ynei+1);
            right ->
              create(TableName,Rad,right,XPoint,YPoint,Xnei+1,Ynei),
              create(TableName,Rad,up,XPoint,YPoint,Xnei,Ynei-1),
              create(TableName,Rad,down,XPoint,YPoint,Xnei,Ynei+1);
            up ->
              create(TableName,Rad,left,XPoint,YPoint,Xnei-1,Ynei),
              create(TableName,Rad,right,XPoint,YPoint,Xnei+1,Ynei),
              create(TableName,Rad,up,XPoint,YPoint,Xnei,Ynei-1);
            down ->
              create(TableName,Rad,left,XPoint,YPoint,Xnei-1,Ynei),
              create(TableName,Rad,right,XPoint,YPoint,Xnei+1,Ynei),
              create(TableName,Rad,down,XPoint,YPoint,Xnei,Ynei+1)
          end;
        true->ok
      end
  end.

addNeighbor(TableName,XPoint,YPoint,Xnei,Ynei) ->
  io:format("{~p,~p} try to add {~p,~p}~n", [XPoint,YPoint,Xnei,Ynei]),
  [{_NameTable,_Location,{Pid,Status, NeighborList}}] = mnesia:dirty_read(TableName, {XPoint,YPoint}),
  Exist = lists:member({Xnei,Ynei},NeighborList),
  if
    Exist == true -> ok;
    true ->
%      NewList = lists:join({Xnei,Ynei},NeighborList),
      io:format("{~p,~p} add{~p,~p}~n", [XPoint,YPoint,Xnei,Ynei]),
      R = #sensorData{x = {XPoint,YPoint}, y ={ Pid, Status, [{Xnei,Ynei}|NeighborList]}},
      mnesia:dirty_write(TableName,R)
  end.



switchOn(TableName,X,Y) ->
  [{_NameTable,Location,{Pid,_Status, NeighborList}}] = mnesia:dirty_read(TableName, {X,Y}),
  R = #sensorData{x =Location, y ={ Pid, true, NeighborList}},
  mnesia:dirty_write(TableName,R).

switchOff(TableName,X,Y) ->
 [{_NameTable,Location,{Pid,Status, NeighborList}}] = mnesia:dirty_read(TableName, {X,Y}),
  R = #sensorData{x =Location, y ={ Pid, not Status, NeighborList}},
  mnesia:dirty_write(TableName,R).


funcTransfer(TableName, {Temp,Wind},SenderLocation,TransferLocation) ->
  Q = mnesia:dirty_read(TableName,TransferLocation),

  [{_NameTable,_Location,{_Pid,_Status, NeighborList}}]= Q,
  if
    TransferLocation == {0,0} -> io:format("{0,0} recive from ~p data:~p~n", [SenderLocation,{Temp,Wind}]);
    true -> Forward = fun(Sensor) -> sendMessage(TableName, {Temp,Wind},SenderLocation,Sensor) end,
      lists:foreach(Forward,NeighborList)
  end.


sendMessage(TableName, {Temp,Wind},SenderLocation ,NextLocation) ->
  Q = mnesia:dirty_read(TableName,NextLocation),
  if
    Q == [] -> ok;%%TODO send to the master
    true -> [{_NameTable,_Location,{NextPid,Status, _NeighborList}}] = Q,
      if
        Status == true -> NextPid ! {{Temp,Wind},SenderLocation};
%%      io:format("~p recive from ~p data:~p~n", [Location, SenderLocation,{Temp,Wind}]);
        true -> ok
      end
  end.



send(MyPid,Msg)->
  gen_server:cast(MyPid,Msg).