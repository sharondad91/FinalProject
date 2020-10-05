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
  code_change/3,send/2]).

-import(sensor,[]).
-define(SERVER, ?MODULE).

%%-record(slaveMaster_state, {}).
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
  List = createNeighbors(Rad,X,Y,{StartX,StartY,EndX,EndY}
  TempSensorPid = sensor:start_sensor(ManagerPid,X,Y),
  R = #sensorData{x ={X,Y}, y ={ TempSensorPid, true, List}},
  mnesia:dirty_write(TableName,R),
  sensor:on(TempSensorPid),
  createTableCol(TableName, ManagerPid,X,Y+1,{StartX,StartY,EndX,EndY});

createTableCol(_TableName,_Rad, _ManagerPid,_X,_Y,_Boundaries) -> ok.

createNeighbors(Rad,X,Y,{StartX,StartY,EndX,EndY},List) when Rad>=1 ->
  Lright = create(Rad,X,Y,X-1,Y,{StartX,StartY,EndX,EndY},List),
  Lup = create(Rad,X,Y,X,Y-1,{StartX,StartY,EndX,EndY},List),
  Lleft = create(Rad,X,Y,X+1,Y,{StartX,StartY,EndX,EndY},List),
  Ldown = create(Rad,X,Y,X,Y+1,{StartX,StartY,EndX,EndY},List),,
  ListSides = lists:merge3(List,Lright,Lleft),
  ListAll = lists:merge3(ListSides,Lup,Ldown),
  NeiList = lists:usort(ListAll).
%%(חוקיים שלא ברשימה(הוסף את כל המשכנים מסביבי
%%שלח לפונקציה את ראד מינוס1 לכל שכן שהוסף
%%עד שראד שווה 0

create(Rad,XPoint,YPoint,Xnei,Ynei,{StartX,StartY,EndX,EndY},List)->
  XPow = math:pow(XPoint-Xnei,2),
  YPow = math:pow(YPoint-Ynei,2),
  RPow = math:pow(Rad,2),
  if
      (XPow+YPow)=< RPow ->
        Lright = create(Rad,XPoint,YPoint,Xnei-1,Ynei,{StartX,StartY,EndX,EndY},[{Xnei,Ynei}|List]),
        Lup = create(Rad,XPoint,YPoint,Xnei,Ynei-1,{StartX,StartY,EndX,EndY},[{Xnei,Ynei}|List]),
        Lleft = create(Rad,XPoint,YPoint,Xnei+1,Ynei,{StartX,StartY,EndX,EndY},[{Xnei,Ynei}|List]),
        Ldown = create(Rad,XPoint,YPoint,Xnei,Ynei+1,{StartX,StartY,EndX,EndY},[{Xnei,Ynei}|List]),
        ListSides = lists:merge3(List,Lright,Lleft),
        ListAll = lists:merge3(ListSides,Lup,Ldown),
        ListAll;
      true->
        List
  end.



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
  lists:join({X-1,Y},List).



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