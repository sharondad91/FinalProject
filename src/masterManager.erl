%%%-------------------------------------------------------------------
%%% @author sharondad
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(masterManager).

-behaviour(gen_server).

-import(dataBaseServer,[]).
-import(display,[]).

-export([start_link/1,send/1,stop/1,aliveAgain/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3,testConditions/2]).

-define(SERVER, ?MODULE).
-define(PC1, 'master@192.168.0.83').
-define(PC2, 'slave2@192.168.0.83').
-define(PC3, 'slave3@192.168.0.83').
-define(PC4, 'slave4@192.168.0.83').%17
-define(PCDisplay, 'display@192.168.0.83').

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

%%start the program with this func, call to init with arg Size
start_link(Size) ->
  gen_server:start_link({global, ?SERVER}, ?MODULE, [Size], []).

%%send message Msg to the master
send(Msg) ->
  gen_server:cast({global, masterManager},Msg).

%%when a slave is revived
aliveAgain(Msg)->
  MonitorPid = global:whereis_name(monitor),
  MonitorPid ! Msg.

%%stop all slaves and terminate himself
stop(Name) ->
  rpc:call(get(pc1),slaveManager,stop,[slaveManager1]),
  rpc:call(get(pc2),slaveManager,stop,[slaveManager2]),
  rpc:call(get(pc3),slaveManager,stop,[slaveManager3]),
  rpc:call(get(pc4),slaveManager,stop,[slaveManager4]),
  rpc:call(get(pcD),display,send,["Terminate"]),
  gen_server:stop(Name).

%%init the program, register the master, spawn monitor the slaves ,connect to the slaves computers
%%saved slaves nodes at dict,
init([Size]) ->
  register(?SERVER,self()),
  rpc:call(?PC1,dataBaseServer,start_db,[]),
  MonitorSlaves = spawn_link(fun() -> slavesMonitor(Size) end),
  global:register_name(monitor,MonitorSlaves),
  NewSize = erlang:trunc(Size/2),
  io:format("now start to start all the slaves ~n"),

  net_kernel:monitor_nodes(true),
  net_kernel:connect_node(?PC2),
  net_kernel:connect_node(?PC3),
  net_kernel:connect_node(?PC4),
  net_kernel:connect_node(?PCDisplay),


  put(pc1, ?PC1),
  put(pc2, ?PC2),
  put(pc3, ?PC3),
  put(pc4, ?PC4),
  put(pcD, ?PCDisplay),


  %%start all slaves, and if there is a problem, stop the others
  OK1 = rpc:call(get(pc1),slaveManager,start_server,[slaveManager1,slaveTable1,tableets1,Size, {0,NewSize-1},{0,NewSize-1},first]),
  case OK1 of
    {ok,Slave1Pid} ->
      MonitorSlaves ! {Slave1Pid,slaveManager1},
      io:format("slave1 started ~p ~n",[{ok,Slave1Pid}]);
    _ ->
      gen_server:stop(?SERVER)
  end,
  OK2 = rpc:call(get(pc2),slaveManager,start_server,[slaveManager2,slaveTable2,tableets2,Size, {NewSize,Size-1},{0,NewSize-1},first]),
  case OK2 of
    {ok,Slave2Pid} ->
      MonitorSlaves ! {Slave2Pid,slaveManager2},
      io:format("slave2 started ~p ~n",[{ok,Slave2Pid}]);
    _ ->
      rpc:call(get(pc1),slaveManager,stop,[slaveManager1]),
      gen_server:stop(?SERVER)
  end,
  OK3 = rpc:call(get(pc3),slaveManager,start_server,[slaveManager3,slaveTable3,tableets3,Size, {0,NewSize-1},{NewSize,Size-1},first]),
  case OK3 of
    {ok,Slave3Pid} ->
      MonitorSlaves ! {Slave3Pid,slaveManager3},
      io:format("slave3 started ~p ~n",[{ok,Slave3Pid}]);
    _ ->
      rpc:call(get(pc1),slaveManager,stop,[slaveManager1]),
      rpc:call(get(pc2),slaveManager,stop,[slaveManager2]),
      gen_server:stop(?SERVER)
  end,
  OK4 = rpc:call(get(pc4),slaveManager,start_server,[slaveManager4,slaveTable4,tableets4,Size, {NewSize,Size-1},{NewSize,Size-1},first]),
  case OK4 of
    {ok,Slave4Pid} ->
      MonitorSlaves ! {Slave4Pid,slaveManager4},
      io:format("slave4 started ~p ~n",[{ok,Slave4Pid}]);
    _ ->
      rpc:call(get(pc1),slaveManager,stop,[slaveManager1]),
      rpc:call(get(pc2),slaveManager,stop,[slaveManager2]),
      rpc:call(get(pc3),slaveManager,stop,[slaveManager3]),
      gen_server:stop(?SERVER)
  end,

  OKD = rpc:call(get(pcD),display,start_link,[Size]),
  case OKD of
    {_Stab,_Stam,_Stan,DisplayPid} ->
      MonitorSlaves ! {DisplayPid,display},
      io:format("Display started ~p ~n",[{ok,DisplayPid}]);
    _ ->
      rpc:call(get(pc1),slaveManager,stop,[slaveManager1]),
      rpc:call(get(pc2),slaveManager,stop,[slaveManager2]),
      rpc:call(get(pc3),slaveManager,stop,[slaveManager3]),
      rpc:call(get(pc4),slaveManager,stop,[slaveManager4]),
      gen_server:stop(?SERVER)
  end,
  {ok,{Size,0,last_x,last_y,MonitorSlaves}}.

handle_call(_Request, _From, {Size,Count,Last_X,Last_Y,MonitorSlaves}) ->
  {reply, ok, {Size,Count,Last_X,Last_Y,MonitorSlaves}}.

%%master get from the slave "Slave ready", and after 3 messages like that send slaves to start all of their sensors
handle_cast({"Slave Ready", TableName}, {Size,Count,Last_X,Last_Y,MonitorSlaves}) ->
  io:format("Master Slave Ready Msg to Table: ~p count ~p  ~n",[TableName,Count]),
  if
    Count==3 ->
      io:format("count==3 -> go to startAllsensors ~n"),
      startAllSensors(),
      io:format("master finish startAllsensors ~n"),
      NewCount = Count;
    true ->
      NewCount = Count+1
  end,
  {noreply,  {Size,NewCount,Last_X,Last_Y,MonitorSlaves}};

%%master received sensor battery dead and update the data base and the display
handle_cast({"Battery dead", {X,Y}}, {Size,Count,Last_X,Last_Y,MonitorSlaves}) ->
  io:format("~p Master: Battery is dead~n",[{X,Y}]),
  dataBaseServer:storeProgramData(programData,{X,Y},{-1,-1,0,0}),
  rpc:call(get(pcD),display,setData,[{X,Y},{-1,-1,0,0}]),
  {noreply,  {Size,Count,Last_X,Last_Y,MonitorSlaves}};

%% sent from monitor to change the key:pcn to Val:?PCn (after disconnect or reconnect one of the slaves)
handle_cast({"UPDATE PC", {KEY,VAL}}, {Size,Count,Last_X,Last_Y,MonitorSlaves}) ->
  put(KEY,VAL),
  {noreply,  {Size,Count,Last_X,Last_Y,MonitorSlaves}};


%%received fire from display that start at time StartTime
%% and send the msg to the relevant slave
handle_cast({"Fire!!!",{X,Y,StartTime}}, {Size,Count,Last_X,Last_Y,MonitorSlaves}) ->
  rpc:call(get(pc1),dataBaseServer,setFire,[StartTime]),
  if
    X<(Size/2) ->
      if
        Y <(Size/2) ->
          rpc:call(get(pc1),slaveManager,send,[slaveManager1,{"Fire!!!",{X,Y}}]);

        true ->
          rpc:call(get(pc3),slaveManager,send,[slaveManager3,{"Fire!!!",{X,Y}}])

      end;
    true ->
      if
        Y <(Size/2) ->
          rpc:call(get(pc2),slaveManager,send,[slaveManager2,{"Fire!!!",{X,Y}}]);

        true ->
          rpc:call(get(pc4),slaveManager,send,[slaveManager4,{"Fire!!!",{X,Y}}])
      end
  end,
  {noreply,  {Size,Count,Last_X,Last_Y,MonitorSlaves}};

%%received fire from one of the slaves (that the sensor is not in his table)
%% and transfer the msg to the relevant slave
handle_cast({"Fire!!!",{X,Y}}, {Size,Count,Last_X,Last_Y,MonitorSlaves}) ->
  if
    X<(Size/2) ->
      if
        Y <(Size/2) ->
          rpc:call(get(pc1),slaveManager,send,[slaveManager1,{"Fire!!!",{X,Y}}]);

        true ->
          rpc:call(get(pc3),slaveManager,send,[slaveManager3,{"Fire!!!",{X,Y}}])

      end;
    true ->
      if
        Y <(Size/2) ->
          rpc:call(get(pc2),slaveManager,send,[slaveManager2,{"Fire!!!",{X,Y}}]);

        true ->
          rpc:call(get(pc4),slaveManager,send,[slaveManager4,{"Fire!!!",{X,Y}}])
      end
  end,
  {noreply,  {Size,Count,Last_X,Last_Y,MonitorSlaves}};


%%received msg "not in my table" about data of one of the sensors
%% from one of the slaves and transfer to the relevant slave
handle_cast({"Not In My Table",SenderLocation, {Temp,Wind,MSGCount,Energy},{X,Y}}, {Size,Count,Last_X,Last_Y,MonitorSlaves}) ->
%%  io:format("Not in my table, from ~p, data:~p~n",[{X,Y},{Temp,Wind}]),
  Cond = testConditions({X,Y},Size),
  if
    Cond == true ->
      if
        X<(Size/2) ->
          if
            Y <(Size/2) ->
              rpc:call(get(pc1),slaveManager,send,[slaveManager1,{"Your Sensor",SenderLocation, {Temp,Wind,MSGCount,Energy},{X,Y}}]);
            true ->
              rpc:call(get(pc3),slaveManager,send,[slaveManager3,{"Your Sensor",SenderLocation, {Temp,Wind,MSGCount,Energy},{X,Y}}])

          end;
        true ->
          if
            Y <(Size/2) ->
              rpc:call(get(pc2),slaveManager,send,[slaveManager2,{"Your Sensor",SenderLocation, {Temp,Wind,MSGCount,Energy},{X,Y}}]);

            true ->
              rpc:call(get(pc4),slaveManager,send,[slaveManager4,{"Your Sensor",SenderLocation, {Temp,Wind,MSGCount,Energy},{X,Y}}])
          end
      end;
    true -> io:format("~p Not in boundaries~n",[{X,Y}])
  end,
  {noreply,  {Size,Count,Last_X,Last_Y,MonitorSlaves}};


%%received msg Exit from the display and stop the program
handle_cast("Exit",  {Size,Count,Last_X,Last_Y,MonitorSlaves}) ->
  stop(?SERVER),
  {noreply,  {Size,Count,Last_X,Last_Y,MonitorSlaves}};


%%received Update data table from the stationary position,
%% update the data table and and update the display
handle_cast({"Update Data Table", {Curr_X,Curr_Y}, {Temp,Wind,MSGCount,Energy}},  {Size,Count,_Last_X,_Last_Y,MonitorSlaves}) ->

  Q = dataBaseServer:getProgramData(programData,{Curr_X,Curr_Y}),
  if
    Q == [] ->
      dataBaseServer:storeProgramData(programData,{Curr_X,Curr_Y},{Temp,Wind,MSGCount,Energy}),
      rpc:call(get(pcD),display,setData,[{Curr_X,Curr_Y},{Temp,Wind,MSGCount,Energy}]),
    if
      {Temp,Wind} == {100,100} ->
        {Burned,StartTime} = dataBaseServer:updateFire(),
        if
          Burned == Size * Size ->

            rpc:call(get(pcD),display,setData,["Fire Time",(erlang:system_time() - StartTime)/1000000000]);
          true-> ok
        end;
      true-> ok
    end;
    true->
      [{_NameTable,_Location,{LastTemp,LastWind,_LastCount,_LastEnergy}}] = Q,
      if
        {LastTemp,LastWind} == {100,100} ->
          ok;
        {LastTemp,LastWind} == {-1,-1} ->
          ok;
        true ->
          dataBaseServer:storeProgramData(programData,{Curr_X,Curr_Y},{Temp,Wind,MSGCount,Energy}),
          rpc:call(get(pcD),display,setData,[{Curr_X,Curr_Y},{Temp,Wind,MSGCount,Energy}]),
          if
            {Temp,Wind} == {100,100} ->
              {Burned,StartTime} = dataBaseServer:updateFire(),
            if
                Burned == Size * Size ->
                  rpc:call(get(pcD),display,setData,["Fire Time",(erlang:system_time() - StartTime)/1000000000]);
                true-> ok
              end;
            true-> ok
          end
      end
  end,
  {noreply,  {Size,Count,Curr_X,Curr_Y,MonitorSlaves}}.



handle_info(_Info,  {Size,Count,Last_X,Last_Y,MonitorSlaves}) ->
  {noreply,  {Size,Count,Last_X,Last_Y,MonitorSlaves}}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn,  {Size,Count,Last_X,Last_Y,MonitorSlaves}, _Extra) ->
  {ok,  {Size,Count,Last_X,Last_Y,MonitorSlaves}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%check if sensor is at the range
testConditions({X,Y},Size)  ->
  if
    ((X>= 0) and (X =< Size-1) and (Y >= 0) and (Y =< Size-1)) ->
      true;
    true ->
      false
  end.

%%send start all sensors Msg to the slaves
startAllSensors() ->
  io:format("inside startAllsensors~n"),
  OK1 = rpc:call(get(pc1),slaveManager,send,[slaveManager1,{"Start All Sensors"}]),
  io:format("slave1 start all sensors: ~p ~n",[OK1]),
  OK2 = rpc:call(get(pc2),slaveManager,send,[slaveManager2,{"Start All Sensors"}]),
  io:format("slave1 start all sensors: ~p ~n",[OK2]),
  OK3 = rpc:call(get(pc3),slaveManager,send,[slaveManager3,{"Start All Sensors"}]),
  io:format("slave1 start all sensors: ~p ~n",[OK3]),
  OK4 = rpc:call(get(pc4),slaveManager,send,[slaveManager4,{"Start All Sensors"}]),
  io:format("slave1 start all sensors: ~p ~n",[OK4]).


%%monitoring the slaves ->
%% if one is ask to reconnect -> stop the backup slave ant init the original
%% if one is down not with a normal Info -> init backup slave instead until he revive
slavesMonitor(Size)->
  receive
    {"aliveAgain",PC} ->
      revive(PC,Size),
      slavesMonitor(Size);
    {SlavePid,SlaveName} ->
      erlang:monitor(process,SlavePid),
      put(SlavePid,SlaveName),
      slavesMonitor(Size);
    {'DOWN',_MonitorRef,_process,SlavePid,normal} ->
      io:format("Slave ~p id exit normal ~n",[SlavePid]),
      slavesMonitor(Size);
    {'DOWN',_MonitorRef,_process,SlavePid,Info} ->
      io:format("Slave ~p id exit with MSG ~p ~n",[SlavePid,Info]),
      restartSlave(SlavePid,Size),
      slavesMonitor(Size);
    Msg ->
      io:format("slavesMonitor get MSG: ~p ~n",[Msg]),
      slavesMonitor(Size)
  end.

%%after one slave is down -> restart backup slave instead and update the dictionary with the relevant node
restartSlave(SlavePid,Size)->
  NewSize = erlang:trunc(Size/2),
  SlaveName = get(SlavePid),
  case SlaveName of
    slaveManager1->
      {ok,SlaveNewPid} = rpc:call(?PC1,slaveManager,start_server,[slaveManager1,slaveTable1,tableets1,Size, {0,NewSize-1},{0,NewSize-1},sec]),
      send({"UPDATE PC", {pc1,?PC1}}),
      io:format("slave1 started ~p ~n",[{ok,SlaveNewPid}]);
    slaveManager2 ->
      {ok,SlaveNewPid} = rpc:call(?PC1,slaveManager,start_server,[slaveManager2,slaveTable2,tableets2,Size, {NewSize,Size-1},{0,NewSize-1},sec]),
      send({"UPDATE PC", {pc2,?PC1}}),
      io:format("slave2 started ~p ~n",[{ok,SlaveNewPid}]);
    slaveManager3 ->
      {ok,SlaveNewPid} = rpc:call(?PC1,slaveManager,start_server,[slaveManager3,slaveTable3,tableets3,Size, {0,NewSize-1},{NewSize,Size-1},sec]),
      send({"UPDATE PC", {pc3,?PC1}}),
      io:format("slave3 started ~p ~n",[{ok,SlaveNewPid}]);
    slaveManager4->
      {ok,SlaveNewPid} = rpc:call(?PC1,slaveManager,start_server,[slaveManager4,slaveTable4,tableets4,Size, {NewSize,Size-1},{NewSize,Size-1},sec]),
      send({"UPDATE PC", {pc4,?PC1}}),
      io:format("slave4 started ~p ~n",[{ok,SlaveNewPid}]);
    display ->
      {_Stab,_Stam,_Stan,DisplayNewPid} = rpc:call(?PC1,display,start_link,[Size]),
      send({"UPDATE PC", {pcD,?PC1}}),
      io:format("display started ~p ~n",[{ok,DisplayNewPid}]);
    _ELSE ->
      io:format("try Restart ~p ~n",[SlaveName])
  end.

%%after a down slave is ask to reconnect -> restart the slave and update the dictionary with the relevant node
revive(PC,Size) ->
  io:format("alive again pc ~p ~n",[PC]),
  NewSize = erlang:trunc(Size/2),
  case PC of
    ?PC1 ->
      rpc:call(?PC1,slaveManager,stop,[slaveManager1]),
      send({"UPDATE PC", {pc1,?PC1}}),
      {ok,SlaveNewPid} = rpc:call(?PC1,slaveManager,start_server,[slaveManager1,slaveTable1,tableets1,Size, {0,NewSize-1},{0,NewSize-1},sec]),
      put(pc1,?PC1),
      erlang:monitor(process,SlaveNewPid),
      put(SlaveNewPid,slaveManager1),
      io:format("slave1 started ~p ~n",[{ok,SlaveNewPid}]);
    ?PC2->
      rpc:call(?PC1,slaveManager,stop,[slaveManager2]),
      send({"UPDATE PC", {pc2,?PC2}}),
      {ok,SlaveNewPid} = rpc:call(?PC2,slaveManager,start_server,[slaveManager2,slaveTable2,tableets2,Size, {NewSize,Size-1},{0,NewSize-1},sec]),
      put(pc2,?PC2),
      erlang:monitor(process,SlaveNewPid),
      put(SlaveNewPid,slaveManager2),
      io:format("slave2 started ~p ~n",[{ok,SlaveNewPid}]);
    ?PC3->
      rpc:call(?PC1,slaveManager,stop,[slaveManager3]),
      send({"UPDATE PC", {pc3,?PC3}}),
      {ok,SlaveNewPid} = rpc:call(?PC3,slaveManager,start_server,[slaveManager3,slaveTable3,tableets3,Size, {0,NewSize-1},{NewSize,Size-1},sec]),
      put(pc3,?PC3),
      erlang:monitor(process,SlaveNewPid),
      put(SlaveNewPid,slaveManager3),
      io:format("slave3 started ~p ~n",[{ok,SlaveNewPid}]);
    ?PC4->
      rpc:call(?PC1,slaveManager,stop,[slaveManager4]),
      send({"UPDATE PC", {pc4,?PC4}}),
      {ok,SlaveNewPid} = rpc:call(?PC4,slaveManager,start_server,[slaveManager4,slaveTable4,tableets4,Size, {NewSize,Size-1},{NewSize,Size-1},sec]),
      put(pc4,?PC4),
      erlang:monitor(process,SlaveNewPid),
      put(SlaveNewPid,slaveManager4),
      io:format("slave4 started ~p ~n",[{ok,SlaveNewPid}]);
    ?PCDisplay->
      rpc:call(?PC1,display,stop,[display]),
      send({"UPDATE PC", {pcD,?PCDisplay}}),
      {_Stab,_Stam,_Stan,SlaveNewPid} = rpc:call(?PCDisplay,display,start_link,[Size]),
      put(pcD,?PCDisplay),
      erlang:monitor(process,SlaveNewPid),
      put(SlaveNewPid,display),
      io:format("display started ~p ~n",[{ok,SlaveNewPid}])
  end.
