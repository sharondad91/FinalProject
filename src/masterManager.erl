%%%-------------------------------------------------------------------
%%% @author sharondad
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(masterManager).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3,testCondisions/2,send/1,whereIsEfi/2,showMeTheData/0,showMe1/3]).

-define(SERVER, ?MODULE).

%%-record(masterManager_state, {}).
-record(sensorData, {x,y}).


%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({global, ?SERVER}, ?MODULE, [40], []).

init([Size]) ->
  register(?SERVER,self()),
  mnesia:create_schema(node()),
  mnesia:start(),
  mnesia:create_table(programData,[{access_mode, read_write}, {type, set}, {record_name, sensorData}, {attributes, record_info(fields, sensorData)}]),
  mnesia:create_table(slaveTable1,[{access_mode, read_write}, {type, set}, {record_name, sensorData}, {attributes, record_info(fields, sensorData)}]),
  mnesia:create_table(slaveTable2,[{access_mode, read_write}, {type, set}, {record_name, sensorData}, {attributes, record_info(fields, sensorData)}]),
  mnesia:create_table(slaveTable3,[{access_mode, read_write}, {type, set}, {record_name, sensorData}, {attributes, record_info(fields, sensorData)}]),
  mnesia:create_table(slaveTable4,[{access_mode, read_write}, {type, set}, {record_name, sensorData}, {attributes, record_info(fields, sensorData)}]),

  %%  להרים את הסופרויזור
  %להרים מהסופרויזר את הסלייבים
  NewSize = erlang:trunc(Size/2),
  slaveManager:start_server(slaveManager1,slaveTable1,tableets1,Size, {0,NewSize-1},{0,NewSize-1}),
  slaveManager:start_server(slaveManager2,slaveTable2,tableets2,Size, {NewSize,Size-1},{0,NewSize-1}),
  slaveManager:start_server(slaveManager3,slaveTable3,tableets3,Size, {0,NewSize-1},{NewSize,Size-1}),
  slaveManager:start_server(slaveManager4,slaveTable4,tableets4,Size, {NewSize,Size-1},{NewSize,Size-1}),

  {ok,{Size,0,last_x,last_y}}.

handle_call(_Request, _From, {Size,Count,Last_X,Last_Y}) ->
  {reply, ok, {Size,Count,Last_X,Last_Y}}.

handle_cast({"Slave Ready", _SlaveName}, {Size,Count,Last_X,Last_Y}) ->
%%  io:format("Master Slave Ready Msg ~n"),
  if
    Count==3 -> startAllSensors(),
      NewCount = Count;
    true -> NewCount = Count+1
  end,
  {noreply,  {Size,NewCount,Last_X,Last_Y}};


handle_cast({"Battery dead", {X,Y}}, {Size,Count,Last_X,Last_Y}) ->
  io:format("~p Battery is dead~n",[{X,Y}]),
  if
    Count==10 -> showMeTheData(),
      NewCount = Count+1;
    true -> NewCount = Count+1
  end,
  R = #sensorData{x ={X,Y}, y ={0,0}},
  mnesia:dirty_write(programData,R),
  {noreply,  {Size,NewCount,Last_X,Last_Y}};

handle_cast({"Not In My Table",SenderLocation, {Temp,Wind},{X,Y}}, {Size,Count,Last_X,Last_Y}) ->
%%  io:format("Not in my table, from ~p, data:~p~n",[{X,Y},{Temp,Wind}]),
  Cond = testCondisions({X,Y},Size),
  if
    Cond == true ->
      if
        X=<(Size/2) ->
          if
            Y =<(Size/2) -> slaveManager:send(slaveManager1,{"Your Sensor",SenderLocation, {Temp,Wind},{X,Y}});
%%              io:format("slaveManager1 Get data to: ~p~n",[{X,Y}]);
            true -> slaveManager:send(slaveManager3,{"Your Sensor",SenderLocation, {Temp,Wind},{X,Y}})
%%              io:format("slaveManager3 Get data to: ~p~n",[{X,Y}])
          end;
        true ->
          if
            Y =<(Size/2) -> slaveManager:send(slaveManager2,{"Your Sensor", SenderLocation, {Temp,Wind},{X,Y}});
%%              io:format("slaveManager2 Get data to: ~p~n",[{X,Y}]);
            true -> slaveManager:send(slaveManager4,{"Your Sensor", SenderLocation, {Temp,Wind},{X,Y}})
%%              io:format("slaveManager4 Get data to: ~p~n",[{X,Y}])
          end
      end;
    true -> io:format("~p Not in boundaries~n",[{X,Y}])
  end,
  {noreply,  {Size,Count,Last_X,Last_Y}};

handle_cast({"Update Data Table", {Last_X, Last_Y}, _Data},  {Size,Count,Last_X,Last_Y}) ->
  {noreply,  {Size,Count,Last_X,Last_Y}};

handle_cast({"Update Data Table", {Curr_X,Curr_Y}, {Temp,Wind}},  {Size,Count,_Last_X,_Last_Y}) ->
%%  io:format("~p Update his data:~p~n",[{Curr_X,Curr_Y},{Temp,Wind}]),
  R = #sensorData{x ={Curr_X,Curr_Y}, y ={Temp,Wind}},
  mnesia:dirty_write(programData,R),
  {noreply,  {Size,Count,Curr_X,Curr_Y}}.


handle_info(_Info,  {Size,Count,Last_X,Last_Y}) ->
  {noreply,  {Size,Count,Last_X,Last_Y}}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn,  {Size,Count,Last_X,Last_Y}, _Extra) ->
  {ok,  {Size,Count,Last_X,Last_Y}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

testCondisions({X,Y},Size)  ->
  if
    ((X>= 0) and (X =< Size-1) and (Y >= 0) and (Y =< Size-1)) ->
      true;
    true ->
      false
  end.

startAllSensors() ->
  slaveManager:send(slaveManager1,{"Start All Sensors"}),
  slaveManager:send(slaveManager2,{"Start All Sensors"}),
  slaveManager:send(slaveManager3,{"Start All Sensors"}),
  slaveManager:send(slaveManager4,{"Start All Sensors"}).

send(Msg) ->
%%  io:format("send func Master recived:~p~n",[Msg]),
  gen_server:cast(masterManager,Msg).

whereIsEfi(X,Y) ->
  Q1 = mnesia:dirty_read(slaveTable1, {X,Y}),
  Q2 = mnesia:dirty_read(slaveTable2,{X,Y}),
  Q3 = mnesia:dirty_read(slaveTable3,{X,Y}),
  Q4 = mnesia:dirty_read(slaveTable4,{X,Y}),
  if
    Q1 =/= [] -> [{_NameTable,_Location,{SensorPid,_Status,_Energy, _NeighborList}}] =Q1,
      SensorPid;
    Q2 =/= [] -> [{_NameTable,_Location,{SensorPid,_Status,_Energy, _NeighborList}}] =Q2,
      SensorPid;
    Q3 =/= [] -> [{_NameTable,_Location,{SensorPid,_Status,_Energy, _NeighborList}}] =Q3,
      SensorPid;
    Q4 =/= [] -> [{_NameTable,_Location,{SensorPid,_Status,_Energy, _NeighborList}}] =Q4,
      SensorPid;
    true -> ok
  end.

showMeTheData() ->
  Q1 = mnesia:dirty_read(programData,{1,1}),
  io:format("~p~n",[Q1]),
  Q2 = mnesia:dirty_read(programData,{2,2}),
  io:format("~p~n",[Q2]),
  Q3 = mnesia:dirty_read(programData,{3,3}),
  io:format("~p~n",[Q3]),
  Q4 = mnesia:dirty_read(programData,{4,4}),
  io:format("~p~n",[Q4]),
  Q5 = mnesia:dirty_read(programData,{5,5}),
  io:format("~p~n",[Q5]).

showMe1(X,Y,Size) when ((X < Size) and (Y < Size))->
  Q1 = mnesia:dirty_read(programData,{X,Y}),
  io:format("~p: ~p~n",[{X,Y},Q1]),
  showMe1(X+1,Y,Size),
  showMe1(X,Y+1,Size);
showMe1(_X,_Y,_Z) ->  ok.




