%%%-------------------------------------------------------------------
%%% @author Owner
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. אוק׳ 2020 12:09
%%%-------------------------------------------------------------------
-module(dataBaseServer).
-author("Owner").

-behaviour(gen_server).

%% API
-export([start_db/0,storeProgramData/3,getProgramData/2,storeSensorData/3,getSensorData/2,
  updateSensorData/3,getAllKeys/1,setFire/1,getFire/0,updateFire/0,msgSent/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(sensorData, {x,y}).            %% sensorData{x ={X,Y} -> coordinates , y ={Pid, isWakeUp ,battery (%),sent, neighbor_list}}
-record(sensorDisplayData, {x,y}).     %% sensorDisplayData{x ={X,Y} -> coordinates , y ={Temp,Wind,Count,Energy} -> dataToDisplay},
%%-record(dataBaseServer_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
start_db() ->
  gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

%%store Key and Val at table TableName (programData)
storeProgramData(TableName,Key,Val)->
  gen_server:cast({global, ?SERVER},{store,TableName,Key,Val}).

%%get Val of key Key from table TableName (program data)
getProgramData(TableName,Key)->
  gen_server:call({global, ?SERVER},{get,TableName,Key}).

%%store Key and Val at table TableName (slaveTable)
storeSensorData(TableName,Key,Val)->
  gen_server:cast({global, ?SERVER},{store,TableName,Key,Val}).

%%get Val=SensorData of key Key from table TableName (slaveTable)
getSensorData(TableName,Key)->
  gen_server:call({global, ?SERVER},{get,TableName,Key}).

%%update data at table TableName at key Key when update type is Update
updateSensorData(TableName,Key,Update)->
  gen_server:cast({global, ?SERVER},{update,TableName,Key,Update}).

%%get all the keys that saved at TableName
getAllKeys(TableName)->
  gen_server:call({global, ?SERVER},{getAll,TableName}).

%%when fire is started -> set fire at the table with the StartTime
setFire(StartTime)->
  gen_server:cast({global, ?SERVER},{update,programData,fire,{time,StartTime}}).

%%check is fire is already started
getFire()->
  gen_server:call({global, ?SERVER},{get,programData,fire}).

%%update counter of burned -> burned++;
updateFire()->
  gen_server:call({global, ?SERVER},{update,programData,fire,burned}).

%%check if Key's(sensor's) msg is sent in this cycle or need to try again
msgSent(TableName,Key)->
  gen_server:cast({global, ?SERVER},{update,TableName,Key,sent}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the dataBaseServer-> create all the tables - 4 slaves table and one program data table

init([]) ->
  register(?SERVER,self()),
  %% create mnesia schema,start mnesia, create all table - 4 slaves tables and 1 program data (to display)
  %% store fire if false (there is no fire now)
  mnesia:create_schema(node()),
  mnesia:start(),
  mnesia:create_table(programData,[{access_mode, read_write}, {type, set}, {record_name, sensorDisplayData}, {attributes, record_info(fields, sensorDisplayData)}]),
  mnesia:create_table(slaveTable1,[{access_mode, read_write}, {type, set}, {record_name, sensorData}, {attributes, record_info(fields, sensorData)}]),
  mnesia:create_table(slaveTable2,[{access_mode, read_write}, {type, set}, {record_name, sensorData}, {attributes, record_info(fields, sensorData)}]),
  mnesia:create_table(slaveTable3,[{access_mode, read_write}, {type, set}, {record_name, sensorData}, {attributes, record_info(fields, sensorData)}]),
  mnesia:create_table(slaveTable4,[{access_mode, read_write}, {type, set}, {record_name, sensorData}, {attributes, record_info(fields, sensorData)}]),
  io:format("All mnesia tables created ~n"),
%%(fire, false,startTime,burned)
  MnesiaTransaction = fun() ->
    R = #sensorDisplayData{x =fire, y = {false,-1,-1}},
    mnesia:write(programData,R,write)
                      end,
  mnesia:transaction(MnesiaTransaction),

  {ok, state}.

%% @private
%% @doc Handling call messages

%%get data from table TableName about key Key
handle_call({get,TableName,Key}, _From, State ) ->
  MnesiaTransaction = fun() ->
    mnesia:read(TableName,Key,read)
                      end,
  {atomic,Data} = mnesia:transaction(MnesiaTransaction),
  {reply, Data, State};

%% update count of burned sensors to burned+1
handle_call({update,programData,fire,burned},_From, State) ->
  MnesiaTransaction = fun() ->
    [{_NameTable,fire,{true,StartTime,LastBurned}}] = mnesia:read(programData,fire,read),
    R = #sensorDisplayData{x =fire, y ={true,StartTime,LastBurned+1}},
    mnesia:write(programData,R,write),
    {LastBurned+1,StartTime}
                      end,
  {atomic,Data} = mnesia:transaction(MnesiaTransaction),
  {reply,Data, State};

%%return all keys of table TableName
handle_call({getAll,TableName}, _From, State ) ->
  MnesiaTransaction = fun() ->
    mnesia:all_keys(TableName)
                      end,
  {atomic,AllKeys} = mnesia:transaction(MnesiaTransaction),
  {reply, AllKeys, State};

handle_call(_Request, _From, State ) ->
  {reply, ok, State}.


%% @private
%% @doc Handling cast messages


%%store data on programData Table
handle_cast({store,programData,Key,Val}, State) ->
  MnesiaTransaction = fun() ->
    R = #sensorDisplayData{x =Key, y =Val},
    mnesia:write(programData,R,write)
                      end,
  mnesia:transaction(MnesiaTransaction),
  {noreply, State};

%%store data on Slave Table
handle_cast({store,TableName,Key,Val}, State) ->
  MnesiaTransaction = fun() ->
    R = #sensorData{x =Key, y =Val},
    mnesia:write(TableName,R,write)
                      end,
  mnesia:transaction(MnesiaTransaction),
  {noreply, State};

%%update fire to true with start-time StartTime
handle_cast({update,programData,fire,{time,StartTime}}, State) ->
  MnesiaTransaction = fun() ->
    R = #sensorDisplayData{x =fire, y ={true,StartTime,0}},
    mnesia:write(programData,R,write)
                      end,
  mnesia:transaction(MnesiaTransaction),
  {noreply, State};



%%update data on slave Table
handle_cast({update,TableName,Key,{add_neighbor,{X,Y}}}, State) ->
  MnesiaTransaction = fun() ->
    [{_NameTable,_Location,{Pid,Status,Energy,Sent , NeighborList}}] = mnesia:read(TableName,Key,read),
    R = #sensorData{x =Key, y ={Pid,Status,Energy,Sent, [{X,Y}|NeighborList]}},
    mnesia:write(TableName,R,write)
                      end,
  mnesia:transaction(MnesiaTransaction),
  {noreply, State};


%%update sensor's status to on
handle_cast({update,TableName,Key,{switchOn,Energy}}, State) ->
  MnesiaTransaction = fun() ->
    [{_NameTable,_Location,{Pid,_Status,_LastEnergy,_LastSent, NeighborList}}] = mnesia:read(TableName,Key,read),
    R = #sensorData{x =Key, y ={Pid,true,Energy,false, NeighborList}},
    mnesia:write(TableName,R,write)
                      end,
  mnesia:transaction(MnesiaTransaction),
  {noreply, State};

%%update sensor's status to on
handle_cast({update,TableName,Key,{switchOff}}, State) ->
  MnesiaTransaction = fun() ->
    [{_NameTable,_Location,{Pid,_Status,Energy,Sent, NeighborList}}] = mnesia:read(TableName,Key,read),
    R = #sensorData{x =Key, y ={Pid,false,Energy,Sent, NeighborList}},
    mnesia:write(TableName,R,write)
                      end,
  mnesia:transaction(MnesiaTransaction),
  {noreply, State};

%%set kill at sensor status - after fire or battery dead
handle_cast({update,TableName,Key,{kill}}, State) ->
  MnesiaTransaction = fun() ->
    [{_NameTable,_Location,{Pid,_Status,_LastEnergy, _LastSent, NeighborList}}] = mnesia:read(TableName,Key,read),
    R = #sensorData{x =Key, y ={Pid,dead,0,true, NeighborList}},
    mnesia:write(TableName,R,write)
                      end,
  mnesia:transaction(MnesiaTransaction),
  {noreply, State};

%%update sensor that his msg was sent
handle_cast({update,TableName,Key,sent}, State) ->
  MnesiaTransaction = fun() ->
    [{_NameTable,_Location,{Pid,Status,Energy, _LastSent, NeighborList}}] = mnesia:read(TableName,Key,read),
    R = #sensorData{x =Key, y ={Pid,Status,Energy,true, NeighborList}},
    mnesia:write(TableName,R,write)
                      end,
  mnesia:transaction(MnesiaTransaction),
  {noreply, State};


%%update sensor pid to NewPid - after restart sensor
handle_cast({update,TableName,Key,{newPid,NewPid}}, State) ->
  MnesiaTransaction = fun() ->
    [{_NameTable,_Location,{_Pid,_Status,Energy,Sent, NeighborList}}] = mnesia:read(TableName,Key,read),
    R = #sensorData{x =Key, y ={NewPid,false,Energy,Sent, NeighborList}},
    mnesia:write(TableName,R,write)
                      end,
  mnesia:transaction(MnesiaTransaction),
  {noreply, State};

handle_cast(_Request, State ) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages

handle_info(_Info, State) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.

terminate(_Reason, _State) ->
  ok.

%% @private
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
