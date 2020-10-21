%%%-------------------------------------------------------------------
%%% @author sharondad
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(slaveManager).

-behaviour(gen_server).

-export([start_server/7,send/2,stop/1,aliveAgain/0
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-import(sensor,[]).
-import(masterManager,[]).
-define(SERVER, ?MODULE).
-define(PC1, 'master@192.168.0.83').
%%-define(PC1, 'master@DESKTOP-1FDI6SC').

%%-record(slaveMaster_state, {}).
%%-record(sensorData, {x,y}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

%%start_server(Name,TableName, {StartX,EndX},{StartY,EndY}) ->
%%  gen_server:start_link({global, Name}, ?MODULE, [TableName, {StartX,EndX},{StartY,EndY}], []).


%%%===================================================================
%%% API
%%%===================================================================

%%master start the slave by this function (call to init func)
start_server(Name,TableName, EtsName,Size, {StartX,EndX},{StartY,EndY},FirstOrSec) ->
  gen_server:start_link({global, Name}, ?MODULE, [Name,TableName,EtsName,Size, {StartX,EndX},{StartY,EndY},FirstOrSec], []).

%%send cast-message Msg by pid to slave with pid MyPid
send(MyPid,Msg) when is_pid(MyPid)->
  gen_server:cast(MyPid,Msg);

%%send cast-message Msg by name Name to slave name Name
send(Name,Msg) when is_atom(Name)->
  gen_server:cast({global,Name},Msg).

%%stop slave with name Name
stop(Name) ->
  gen_server:stop(Name).

%%after exit with value disconnect - reconnect to the master and take back control of his part
aliveAgain() ->
  rpc:call(?PC1,masterManager,aliveAgain,[{"aliveAgain", node()}]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%init slave with args Name (to registered), Mnesia Table name (TableName),
%% Ets table name, Size of the board, his coordinates,
%% First time of init or Sec(or more) -> after or before disconnect
init([Name,TableName,EtsName,Size, {StartX,EndX},{StartY,EndY},FirstOrSec]) ->
  case FirstOrSec of
    %%First -> at the init of the program
    first ->
      %%register, open his ets, spawn monitor for the sensors create the sensors,
      %%update the master that slave is ready
      register(Name,self()),
      MyPid = self(),
      MonitorEts = ets:new(EtsName,[set,named_table,public]),
      MonitorPid = spawn_link(fun() -> sensorsMonitor(TableName,MyPid,MonitorEts) end),
      io:format("~p registered, open ets, spawn monitor. Now start open sensors ~n",[Name]),
      createTableRow(TableName,MonitorPid,Size,4,MyPid,StartX,StartY,{StartX,StartY,EndX,EndY},FirstOrSec),
      OK = rpc:call(?PC1,masterManager,send,[{"Slave Ready", TableName}]),
      io:format("all sensors store in ~p sent 'Slave Ready' to master return val - ~p ~n",[ TableName,OK]),
      Center = erlang:trunc(Size/2)-1,
      {ok, {TableName,Center}};

    %%sec -> after someone disconnected the Master or the Slave (depends on the situation) init the slave part again
    sec->
      %%register, open his ets, spawn monitor for the sensors create the sensors,
      %%update the master that slave is ready
      register(Name,self()),
      MyPid = self(),
      MonitorEts = ets:new(EtsName,[set,named_table,public]),
      MonitorPid = spawn_link(fun() -> sensorsMonitor(TableName,MyPid,MonitorEts) end),
      io:format("restart ~p registered, open ets, spawn monitor. Now start open sensors ~n",[Name]),
      createTableRow(TableName,MonitorPid,Size,4,MyPid,StartX,StartY,{StartX,StartY,EndX,EndY},FirstOrSec),
      startAllSensors(TableName),
      Center = erlang:trunc(Size/2)-1,
      io:format("restart success pid ~p ~n",[MyPid]),
      {ok, {TableName,Center}};
    true ->
      io:format("init isn't First or Sec is ~p ~n",[FirstOrSec])
  end.


handle_call(_Request, _From, State) ->
  {reply, ok, State}.


%% handle with Sensor data msg -> data is {Temp,Wind,Count,Energy}
%% about sensor SenderLocation
%% TransferLocation is the current station that have the msg
%% now the slave have to transfer to all his neighbors
handle_cast({{Temp,Wind,Count,Energy},SenderLocation,TransferLocation}, {TableName, Center}) ->
  %% if fire is on the board Bool is true
  %% if the is fire -> dont send other msg
  %% else -> transfer the msg
  [{_Table,fire, {Bool,_StartTime,_Burned}}] = rpc:call(?PC1,dataBaseServer,getFire,[]),
  if
    Bool == false ->
      funcTransfer(TableName,{Temp,Wind,Count,Energy},SenderLocation,TransferLocation,Center),
      {noreply, {TableName, Center}};

    Bool == true ->
      {noreply, {TableName, Center}};

    true ->
      {noreply, {TableName, Center}}
  end;


%%sensor is wake up -> update slave data table
handle_cast({"I woke up", {X,Y},Energy}, {TableName, Center}) ->
  switchOn(TableName,X,Y,Energy),
  {noreply, {TableName, Center}};

%%sensor went to sleep -> update slave data table
handle_cast({"Im going to sleep", {X,Y}}, {TableName, Center}) ->
  switchOff(TableName,X,Y),
  {noreply, {TableName, Center}};

%%sensor battery dead -> update master and slave data table
handle_cast({"Battery dead", {X,Y}}, {TableName, Center}) ->
  rpc:call(?PC1,masterManager,send,[{"Battery dead", {X,Y}}]),
  rpc:call(?PC1,dataBaseServer,updateSensorData,[TableName,{X,Y},{kill}]),
  [{_NameTable,_Location,{SensorPid,_Status,_Energy,_Sent, _NeighborList}}] = rpc:call(?PC1,dataBaseServer,getSensorData,[TableName,{X,Y}]),
  sensor:stop(SensorPid),
  {noreply, {TableName, Center}};

%%after all the slaves are ready, master send confirmation to Start all sensors
%%make all the sensors to on
handle_cast({"Start All Sensors"}, {TableName, Center}) ->
  startAllSensors(TableName),
  {noreply, {TableName, Center}};

%%slave get msg from master to transfer msg to one of his sensors( NextLocation)
handle_cast({"Your Sensor", SenderLocation, {Temp,Wind,Count,Energy},NextLocation}, {TableName, Center}) ->
  sendMessage(TableName,{Temp,Wind,Count,Energy},SenderLocation,NextLocation,NextLocation,Center),
  {noreply, {TableName, Center}};

%%Fire!!!! run for your life
%%now seriously, get this msg from master (after mouse click on fire button)
%%send it to relevant sensor
handle_cast({"Fire!!!", {X,Y}}, {TableName, Center}) ->
  Q = rpc:call(?PC1,dataBaseServer,getSensorData,[TableName,{X,Y}]),
  if
    Q ==[] -> ok;
    true ->
      [{_NameTable,_Location,{SensorPid,_Status,_Energy,_Sent, _NeighborList}}] = Q ,
      SensorPid ! "Fire!!!"
  end,
  {noreply, {TableName, Center}};

%%get this fire msg from sensor -> update master about the fire
%% and send it to his neighbors
handle_cast({"Sensor on Fire!!!", {X,Y}}, {TableName, Center}) ->
  funcTransfer(TableName,"Fire Alert",{X,Y},Center),
  {noreply, {TableName, Center}};

handle_cast(_Other, {TableName, Center}) ->
  {noreply, {TableName, Center}}.


handle_info(_Info, State) ->
  {noreply, State}.

%%terminate his sensors and himself
terminate(_Reason, {TableName, _Center}) ->
  AllKeys= rpc:call(?PC1,dataBaseServer,getAllKeys,[TableName]),
  Forward = fun(Key) -> stopSensor(TableName, Key) end,
  lists:foreach(Forward,AllKeys),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%createTableRow and createTableCol are function that runs over all the relevant row and cols
%%and create sensor at each location
createTableRow(TableName,Monitor,Size,Rad, ManagerPid,X,Y,{StartX,StartY,EndX,EndY},FirstOrSec) when X =< EndX  ->
  createTableCol(TableName,Monitor,Size,Rad, ManagerPid,X,Y,{StartX,StartY,EndX,EndY},FirstOrSec),
  createTableRow(TableName,Monitor,Size,Rad, ManagerPid,X+1,Y,{StartX,StartY,EndX,EndY},FirstOrSec);

createTableRow(_TableName,_Monitor,_Size,_Rad, _ManagerPid,_X,_Y,_Boundaries,_FirstOrSec) -> ok.

createTableCol(TableName,Monitor,Size,Rad, ManagerPid,X,Y,{StartX,StartY,EndX,EndY},FirstOrSec) when Y=< EndY ->
  Center = erlang:trunc(Size/2)-1,
  TempSensorPid = sensor:start_sensor(ManagerPid,X,Y,Center),
  Monitor ! {TempSensorPid,{X,Y}},
  case FirstOrSec of
    first ->
      rpc:call(?PC1,dataBaseServer,storeSensorData,[TableName,{X,Y},{ TempSensorPid, true,100,false, []}]),
      createNeighbors(TableName,Size,Rad,X,Y);
    sec->
      rpc:call(?PC1,dataBaseServer,updateSensorData,[TableName,{X,Y},{newPid,TempSensorPid}]);
    true->
      io:format("FirstOrSec isnt first or sec")
  end,
  createTableCol(TableName,Monitor,Size,Rad, ManagerPid,X,Y+1,{StartX,StartY,EndX,EndY},FirstOrSec);

createTableCol(_TableName,_Monitor,_Size,_Rad, _ManagerPid,_X,_Y,_Boundaries,_FirstOrSec) -> ok.


%%after create each sensor find all the sensor's neighbors by radius rad
createNeighbors(TableName,Size,Rad,X,Y) when Rad>=1 ->
  create(TableName,Size,Rad,right,X,Y,X+1,Y),
  create(TableName,Size,Rad,up,X,Y,X,Y-1),
  create(TableName,Size,Rad,left,X,Y,X-1,Y),
  create(TableName,Size,Rad,down,X,Y,X,Y+1),
  rpc:call(?PC1,dataBaseServer,getSensorData,[TableName,{X,Y}]).

%%recursive function of create neighbors
create(TableName,Size,Rad,From,XPoint,YPoint,Xnei,Ynei)->
  Good = testConditions({Xnei,Ynei},Size),
  if
    Good == true ->
      [{_NameTable,_Location,{_Pid,_Status,_Energy,_Sent, NeighborList}}] = rpc:call(?PC1,dataBaseServer,getSensorData,[TableName,{XPoint,YPoint}]),
      Exist = lists:member({Xnei,Ynei},NeighborList),
      if
        Exist == true -> [];
        true ->XPow = math:pow(XPoint-Xnei,2),
          YPow = math:pow(YPoint-Ynei,2),
          RPow = math:pow(Rad,2),
          if
            (XPow+YPow)=< RPow ->
              addNeighbor(TableName,XPoint,YPoint,Xnei,Ynei),
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

%%add the neighbor to the neighbor list
addNeighbor(TableName,XPoint,YPoint,Xnei,Ynei) ->
  [{_NameTable,_Location,{_Pid,_Status,_Energy,_Sent, NeighborList}}] = rpc:call(?PC1,dataBaseServer,getSensorData,[TableName,{XPoint,YPoint}]),
  Exist = lists:member({Xnei,Ynei},NeighborList),
  if
    Exist == true -> ok;
    true ->
      rpc:call(?PC1,dataBaseServer,updateSensorData,[TableName,{XPoint,YPoint},{add_neighbor,{Xnei,Ynei}}])
  end.

%%start all sensors after receive msg from the master
startAllSensors(TableName) ->
  AllKeys= rpc:call(?PC1,dataBaseServer,getAllKeys,[TableName]),
  Forward = fun(Key) -> startSensor(TableName, Key) end,
  lists:foreach(Forward,AllKeys).

%%set sensor to on
startSensor(TableName,{X,Y}) ->
  [{_NameTable,_Location,{SensorPid,_Status,_Energy,_Sent, _NeighborList}}] = rpc:call(?PC1,dataBaseServer,getSensorData,[TableName,{X,Y}]),
  sensor:on(SensorPid).

%%when sensor wake up update data table
switchOn(TableName,X,Y,Energy) ->
  rpc:call(?PC1,dataBaseServer,updateSensorData,[TableName,{X,Y},{switchOn,Energy}]).

%%when sensor go to sleep update data table
switchOff(TableName,X,Y) ->
  rpc:call(?PC1,dataBaseServer,updateSensorData,[TableName,{X,Y},{switchOff}]).

%%transfer msg "fire alert" to all the neighbors of the sensor at location {X,Y} (all direction)
funcTransfer(TableName,"Fire Alert",{X,Y},Center)  ->
  [{_NameTable,_Location,{Pid,_Status,_Energy,_Sent, NeighborList}}] = rpc:call(?PC1,dataBaseServer,getSensorData,[TableName, {X,Y}]),
  rpc:call(?PC1,masterManager,send,[{"Update Data Table", {X,Y},{100,100,0,0}}]),
  sensor:stop(Pid),
  rpc:call(?PC1,dataBaseServer,updateSensorData,[TableName,{X,Y},{kill}]),
  Forward = fun(Sensor) -> sendMessageToAll(TableName,"Fire Alert",Sensor,Center) end,
  lists:foreach(Forward,NeighborList).

%%transfer Data msg to all the neighbors of the sensor at location {X,Y} (relevant direction)
funcTransfer(TableName, {Temp,Wind,Count,Energy},SenderLocation,TransferLocation,Center) ->
  [{_NameTable,_Location,{_Pid,_Status,_LastEnergy,_Sent, NeighborList}}] = rpc:call(?PC1,dataBaseServer,getSensorData,[TableName,TransferLocation]),

  CenterState = lists:member({Center,Center},NeighborList),
  if
    CenterState == true ->
      sendMessage(TableName, {Temp,Wind,Count,Energy},SenderLocation,TransferLocation,{Center,Center},Center);
    true ->
      Forward = fun(Sensor) -> sendMessage(TableName, {Temp,Wind,Count,Energy},SenderLocation,TransferLocation,Sensor,Center) end,
      lists:foreach(Forward,NeighborList),
      if
      %%double check relevant only if this is from the Sender Sensor
        TransferLocation == SenderLocation->
          [{_NameTable,_Location,{_Pid,_Status,_LastEnergy,Sent, NeighborList}}] = rpc:call(?PC1,dataBaseServer,getSensorData,[TableName,TransferLocation]),
          if
            %didnt sent -> try one more time
            Sent == false ->
              lists:foreach(Forward,NeighborList);
            true -> ok
          end;
        true -> ok
      end
  end.

%%send Data msg to one (NextLocation) of the neighbors of the sensor (TransferLocation)
%% if he is wake up and at the relevant direction
sendMessage(TableName, {Temp,Wind,Count,Energy},SenderLocation,TransferLocation,NextLocation,Center) ->
  Q = rpc:call(?PC1,dataBaseServer,getSensorData,[TableName,NextLocation]),
  if
    Q == [] ->
      rpc:call(?PC1,masterManager,send,[{"Not In My Table",SenderLocation, {Temp,Wind,Count,Energy},NextLocation}]);
    true ->
      [{_NameTable,_Location,{NextPid,Status,_LastEnergy,_Sent, _NeighborList}}] = Q,
      Better = closerFunc(TransferLocation,NextLocation,Center),
      if
        (Status == true) and (Better == true) ->
          NextPid ! {{Temp,Wind,Count,Energy},SenderLocation},
          if
            %%double check relevant only if this is from the Sender Sensor
            TransferLocation == SenderLocation->
              rpc:call(?PC1,dataBaseServer,msgSent,[TableName,SenderLocation,true]);
            true -> ok
          end;
        true -> ok
      end
  end.

%%send fire msg to one (Sensor) of the neighbors of the sensor only if he is alive
sendMessageToAll(TableName,"Fire Alert",Sensor,Center) ->
  Q = rpc:call(?PC1,dataBaseServer,getSensorData,[TableName,Sensor]),
  if
    Q == [] ->
      rpc:call(?PC1,masterManager,send,[{"Fire!!!",Sensor}]);

    true ->
      [{_NameTable,Location,{NextPid,Status,_Energy,_Sent, _NeighborList}}] = Q,
      if
        Location == {Center,Center}->
          NextPid ! "Fire!!!",
          rpc:call(?PC1,masterManager,send,[{"Update Data Table", Location,{100,100,0,0}}]);
        true->
          if
            Status==dead ->
              ok;
            true ->   NextPid ! "Fire!!!"
          end
      end
  end.


%%monitoring the sensors if one is down not with normal reason -> init new one instead
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

%%init new sensor with the old one data and properties
restartSensor(TableName, SensorPid,ManagerPid,MonitorEts) ->
  [{_Sens,{X,Y}}] = ets:lookup(MonitorEts,SensorPid),
  ets:delete(MonitorEts,SensorPid),
  [{_NameTable,_Location,{_Pid,_Status,Energy,_Sent,_NeighborList}}] = rpc:call(?PC1,dataBaseServer,getSensorData,[TableName,{X,Y}]),
  NewPid = sensor:start_sensor(ManagerPid,X,Y,Energy),
  erlang:monitor(process,NewPid),
  rpc:call(?PC1,dataBaseServer,updateSensorData,[TableName,{X,Y},{newPid,NewPid}]),
  ets:insert(MonitorEts,{NewPid,{X,Y}}),
  io:format("~p restart and his Energy is ~p~n",[{X,Y},Energy]),
  startSensor(TableName,{X,Y}).


%%check if the location is inside the table
testConditions({X,Y},Size)  ->
  if
    ((X>= 0) and (X =< Size-1) and (Y >= 0) and (Y =< Size-1)) ->
      true;
    true ->
      false
  end.

%%check if the next sensor is closer than the current sensor
closerFunc({OldX,OldY}, {NewX,NewY},Center) ->
  if
    (abs(Center-NewX) =< abs(Center-OldX)) and (abs(Center-NewY) =< abs(Center-OldY)) ->
      true;
    true ->
      false
  end.

%%if slave is terminate he stop all his sensors with this function
stopSensor(TableName, Key)->
  [{_NameTable,_Location,{Pid,Status,_Energy,_Sent,_NeighborList}}] = rpc:call(?PC1,dataBaseServer,getSensorData,[TableName,Key]),
  if
    Status =/= dead ->
      sensor:stop(Pid);
    true -> ok
  end.