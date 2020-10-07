%%%-------------------------------------------------------------------
%%% @author sharondad
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(slaveManager).

-behaviour(gen_server).

-export([start_server/6]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3,send/2]).

-import(sensor,[]).
-import(masterManager,[]).
-define(SERVER, ?MODULE).

%%-record(slaveMaster_state, {}).
-record(sensorData, {x,y}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

%%start_server(Name,TableName, {StartX,EndX},{StartY,EndY}) ->
%%  gen_server:start_link({global, Name}, ?MODULE, [TableName, {StartX,EndX},{StartY,EndY}], []).
start_server(Name,TableName, EtsName,Size, {StartX,EndX},{StartY,EndY}) ->
  gen_server:start_link({global, Name}, ?MODULE, [Name,TableName,EtsName,Size, {StartX,EndX},{StartY,EndY}], []).


init([Name,TableName,EtsName,Size, {StartX,EndX},{StartY,EndY}]) ->
  register(Name,self()),
  MyPid = self(),
%%  mnesia:create_schema(node()),
%%  mnesia:start(),
%%  mnesia:create_table(TableName,[{access_mode, read_write}, {type, set}, {record_name, sensorData}, {attributes, record_info(fields, sensorData)}]),
  MonitorEts = ets:new(EtsName,[set,named_table,public]),
  MonitorPid = spawn_link(fun() -> sensorsMonitor(TableName,MyPid,MonitorEts) end),
  createTableRow(TableName,MonitorPid,Size,3,MyPid,StartX,StartY,{StartX,StartY,EndX,EndY}),
%%  gen_server:cast(masterManager,{"Slave Ready", TableName}),
  masterManager:send({"Slave Ready", TableName}),
%%  io:format("Slave ~p Is Ready~n",[TableName]),
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
  gen_server:cast(masterManager,{"Battery dead", {X,Y}}),
  [{_NameTable,_Location,{SensorPid,_Status, _NeighborList}}] = mnesia:dirty_read(TableName, {X,Y}),
  sensor:stop(SensorPid),
  {noreply, TableName};

handle_cast({"Start All Sensors"}, TableName) ->
  startAllSensors(TableName),
  {noreply, TableName};

handle_cast({"Your Sensor", SenderLocation, {Temp,Wind},NextLocation}, TableName) ->
  sendMessage(TableName,{Temp,Wind},SenderLocation,NextLocation,NextLocation),
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

createTableRow(TableName,Monitor,Size,Rad, ManagerPid,X,Y,{StartX,StartY,EndX,EndY}) when X =< EndX  ->
  createTableCol(TableName,Monitor,Size,Rad, ManagerPid,X,Y,{StartX,StartY,EndX,EndY}),
  createTableRow(TableName,Monitor,Size,Rad, ManagerPid,X+1,Y,{StartX,StartY,EndX,EndY});

createTableRow(_TableName,_Monitor,_Size,_Rad, _ManagerPid,_X,_Y,_Boundaries) -> ok.

createTableCol(TableName,Monitor,Size,Rad, ManagerPid,X,Y,{StartX,StartY,EndX,EndY}) when Y=< EndY ->
  TempSensorPid = sensor:start_sensor(ManagerPid,X,Y),
  Monitor ! {TempSensorPid,{X,Y}},
  R = #sensorData{x ={X,Y}, y ={ TempSensorPid, true, []}},
  mnesia:dirty_write(TableName,R),
  createNeighbors(TableName,Size,Rad,X,Y),
  createTableCol(TableName,Monitor,Size,Rad, ManagerPid,X,Y+1,{StartX,StartY,EndX,EndY});

createTableCol(_TableName,_Monitor,_Size,_Rad, _ManagerPid,_X,_Y,_Boundaries) -> ok.

createNeighbors(TableName,Size,Rad,X,Y) when Rad>=1 ->
%%  io:format("create neighbors for {~p,~p}~n",[X,Y]),
  create(TableName,Size,Rad,right,X,Y,X+1,Y),
%%  io:format("create Right neighbors~n"),
  create(TableName,Size,Rad,up,X,Y,X,Y-1),
%%  io:format("create UP neighbors~n"),
  create(TableName,Size,Rad,left,X,Y,X-1,Y),
%%  io:format("create Left neighbors~n"),
  create(TableName,Size,Rad,down,X,Y,X,Y+1),
  mnesia:dirty_read(TableName, {X,Y}).
%%  io:format("{~p,~p} data is : ~p~n",[X,Y,Q]).



create(TableName,Size,Rad,From,XPoint,YPoint,Xnei,Ynei)->
  Good = testCondisions({Xnei,Ynei},Size),
  if
    Good == true -> [{_NameTable,_Location,{_Pid,_Status, NeighborList}}] = mnesia:dirty_read(TableName, {XPoint,YPoint}),
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
                  create(TableName,Size,Rad,left,XPoint,YPoint,Xnei-1,Ynei),
                  create(TableName,Size,Rad,up,XPoint,YPoint,Xnei,Ynei-1),
                  create(TableName,Size,Rad,down,XPoint,YPoint,Xnei,Ynei+1);
                right ->
                  create(TableName,Size,Rad,right,XPoint,YPoint,Xnei+1,Ynei),
                  create(TableName,Size,Rad,up,XPoint,YPoint,Xnei,Ynei-1),
                  create(TableName,Size,Rad,down,XPoint,YPoint,Xnei,Ynei+1);
                up ->
                  create(TableName,Size,Rad,left,XPoint,YPoint,Xnei-1,Ynei),
                  create(TableName,Size,Rad,right,XPoint,YPoint,Xnei+1,Ynei),
                  create(TableName,Size,Rad,up,XPoint,YPoint,Xnei,Ynei-1);
                down ->
                  create(TableName,Size,Rad,left,XPoint,YPoint,Xnei-1,Ynei),
                  create(TableName,Size,Rad,right,XPoint,YPoint,Xnei+1,Ynei),
                  create(TableName,Size,Rad,down,XPoint,YPoint,Xnei,Ynei+1)
              end;
            true->ok
          end
      end;
    true -> ok
  end.

addNeighbor(TableName,XPoint,YPoint,Xnei,Ynei) ->
%%  io:format("{~p,~p} try to add {~p,~p}~n", [XPoint,YPoint,Xnei,Ynei]),
  [{_NameTable,_Location,{Pid,Status, NeighborList}}] = mnesia:dirty_read(TableName, {XPoint,YPoint}),
  Exist = lists:member({Xnei,Ynei},NeighborList),
  if
    Exist == true -> ok;
    true ->
%      NewList = lists:join({Xnei,Ynei},NeighborList),
%%      io:format("{~p,~p} add{~p,~p}~n", [XPoint,YPoint,Xnei,Ynei]),
      R = #sensorData{x = {XPoint,YPoint}, y ={ Pid, Status, [{Xnei,Ynei}|NeighborList]}},
      mnesia:dirty_write(TableName,R)
  end.

startAllSensors(TableName) ->
  AllKeys = mnesia:dirty_all_keys(TableName),
  Forward = fun(Key) -> startSensor(TableName, Key) end,
  lists:foreach(Forward,AllKeys).

startSensor(TableName,{X,Y}) ->
  [{_NameTable,_Location,{TempSensorPid,_Status, _NeighborList}}] = mnesia:dirty_read(TableName, {X,Y}),
  sensor:on(TempSensorPid).

switchOn(TableName,X,Y) ->
  [{_NameTable,Location,{Pid,_Status, NeighborList}}] = mnesia:dirty_read(TableName, {X,Y}),
  R = #sensorData{x =Location, y ={ Pid, true, NeighborList}},
  mnesia:dirty_write(TableName,R).

switchOff(TableName,X,Y) ->
 [{_NameTable,Location,{Pid,Status, NeighborList}}] = mnesia:dirty_read(TableName, {X,Y}),
  R = #sensorData{x =Location, y ={ Pid, not Status, NeighborList}},
  mnesia:dirty_write(TableName,R).

funcTransfer(TableName, {Temp,Wind},SenderLocation,TransferLocation) ->
  [{_NameTable,_Location,{_Pid,_Status, NeighborList}}] = mnesia:dirty_read(TableName,TransferLocation),
  Forward = fun(Sensor) -> sendMessage(TableName, {Temp,Wind},SenderLocation,TransferLocation,Sensor) end,
  lists:foreach(Forward,NeighborList).
  

sendMessage(TableName, {Temp,Wind},SenderLocation,TransferLocation,NextLocation) ->
  Q = mnesia:dirty_read(TableName,NextLocation),
  if
    Q == [] -> masterManager:send({"Not In My Table",SenderLocation, {Temp,Wind},NextLocation});
    true -> [{_NameTable,_Location,{NextPid,Status, _NeighborList}}] = Q,
      Better = closerFunc(TransferLocation,NextLocation),
      if
        (Status == true) and (Better == true) -> NextPid ! {{Temp,Wind},SenderLocation};
%%      io:format("~p recive from ~p data:~p~n", [Location, SenderLocation,{Temp,Wind}]);
        true -> ok
      end
  end.

send(MyPid,Msg) when is_pid(MyPid)->
  gen_server:cast(MyPid,Msg);

send(Name,Msg) when is_atom(Name)->
  gen_server:cast(Name,Msg).

sensorsMonitor(TableName,ManagerPid,MonitorEts) ->
  receive
    {SensorPid,Location} ->
      erlang:monitor(process,SensorPid),
      ets:insert(MonitorEts,{SensorPid,Location}),
      sensorsMonitor(TableName,ManagerPid,MonitorEts)  ;
    {'DOWN',_MonitorRef,_process,_SensorPid,normal} ->
      sensorsMonitor(TableName,ManagerPid,MonitorEts)  ;
    {'DOWN',_MonitorRef,_process,SensorPid,_Other} ->
      restartSensor(TableName,SensorPid,ManagerPid,MonitorEts),
      sensorsMonitor(TableName,ManagerPid,MonitorEts);
    _Msg ->
      sensorsMonitor(TableName,ManagerPid,MonitorEts)
  end.

restartSensor(TableName, SensorPid,ManagerPid,MonitorEts) ->
  [{_Sens,{X,Y}}] = ets:lookup(MonitorEts,SensorPid),
  ets:delete(MonitorEts,SensorPid),
  [{_NameTable,_Location,{_Pid,_Status, NeighborList}}] = mnesia:dirty_read(TableName, {X,Y}),
  NewPid = sensor:start_sensor(ManagerPid,X,Y),
  erlang:monitor(process,NewPid),
  R = #sensorData{x ={X,Y}, y ={ NewPid, false, NeighborList}},
  mnesia:dirty_write(TableName,R),
  ets:insert(MonitorEts,{NewPid,{X,Y}}),
  startSensor(TableName,{X,Y}).

testCondisions({X,Y},Size)  ->
  if
    ((X>= 0) and (X =< Size-1) and (Y >= 0) and (Y =< Size-1)) ->
      true;
    true ->
      false
  end.

closerFunc({OldX,OldY}, {NewX,NewY}) ->
  if
    (NewX =< OldX) or (NewY =< OldY) -> true;
    true -> false
  end.