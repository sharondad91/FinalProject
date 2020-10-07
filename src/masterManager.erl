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
  code_change/3,testCondisions/2,send/1,whereIsEfi/2]).

-define(SERVER, ?MODULE).

%%-record(masterManager_state, {}).
-record(sensorData, {x,y}).


%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({global, ?SERVER}, ?MODULE, [6], []).

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

  {ok,{Size,0}}.

handle_call(_Request, _From, {Size,Count}) ->
  {reply, ok, {Size,Count}}.

handle_cast({"Slave Ready", _SlaveName}, {Size,Count}) ->
%%  io:format("Master Slave Ready Msg ~n"),
  if
    Count==3 -> startAllSensors(),
      NewCount = Count;
    true -> NewCount = Count+1
  end,
  {noreply,  {Size,NewCount}};


handle_cast({"Battery dead", {X,Y}}, {Size,Count}) ->
  io:format("~p Battery is dead~n",[{X,Y}]),
  R = #sensorData{x ={X,Y}, y ={0,0}},
  mnesia:dirty_write(programData,R),
  {noreply,  {Size,Count}};

handle_cast({"Not In My Table",SenderLocation, {Temp,Wind},{X,Y}}, {Size,Count}) ->
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
  {noreply,  {Size,Count}};

handle_cast({"Update Data Table",SenderLocation, {Temp,Wind}},  {Size,Count}) ->
  io:format("~p Update his data:~p~n",[SenderLocation,{Temp,Wind}]),
  R = #sensorData{x =SenderLocation, y ={Temp,Wind}},
  mnesia:dirty_write(programData,R),
  {noreply,  {Size,Count}}.


handle_info(_Info,  {Size,Count}) ->
  {noreply,  {Size,Count}}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn,  {Size,Count}, _Extra) ->
  {ok,  {Size,Count}}.

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
    Q1 =/= [] -> io:format("slaveTable1~n");
    Q2 =/= [] -> io:format("slaveTable2~n");
    Q3 =/= [] -> io:format("slaveTable3~n");
    Q4 =/= [] -> io:format("slaveTable4~n");
    true -> ok
  end.
