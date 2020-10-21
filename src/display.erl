%%%-------------------------------------------------------------------
%%% @author sharondad
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(display).

-behaviour(wx_object).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2,handle_event/2, handle_info/2, terminate/2,
  code_change/3,setData/2,send/1,stop/1,aliveAgain/0]).

-include_lib("wx/include/wx.hrl").

-define(SERVER, ?MODULE).

-define(max_x,(600)).
-define(max_y,(600)).

-define(PC1, 'master@192.168.0.83').

-record(state,
{
  frame,
  parent,
  grid,
  x,
  y,
  ets
}).

%%-record(display_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================
%%start the display, call to init with arg Size
start_link(Size) ->
  wx_object:start_link({global,?SERVER},?MODULE,[Size],[]).

%%send message Msg to display
send(Msg) ->
  whereis(display) ! Msg.

%%stop display
stop(_Name) ->
  wx_object:stop(whereis(display)).

%%reconnect to the master after revive
aliveAgain() ->
  rpc:call(?PC1,masterManager,aliveAgain,[{"aliveAgain", node()}]).

%%build the GUI, put buttons and grid.
init([Size]) ->
  register(display,self()),
  put(display,self()),
  DisplayEts = ets:new(displayets,[set]),
  %% Create Board
  WxServer = wx:new(),
  Frame = wxFrame:new(WxServer, ?wxID_ANY, "Sensors Map", [{size,{?max_x, ?max_y}}]),
  Panel  = wxPanel:new(Frame),

  %% Create Buttons
  DataButton = wxButton:new(Panel, 1, [{label, "Data"}]),
  wxPanel:connect(DataButton, command_button_clicked),

  %% Create a wxSpinCtrl with range between 0 and 200
  SpinSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,[]),
  SpinCtrlRow = wxSpinCtrl:new(Panel, [{id, 1},{pos, {100,20}},{size, {70,30}}]),
  wxSpinCtrl:setRange(SpinCtrlRow, 0, 199),
  wxSpinCtrl:setToolTip(SpinCtrlRow, "A range from 0 to 200"),
  wxSpinCtrl:connect(SpinCtrlRow,command_spinctrl_updated),
  SpinCtrlCol = wxSpinCtrl:new(Panel, [{id, 2}, {pos, {170,20}},{size, {70,30}}]),
  wxSpinCtrl:setRange(SpinCtrlCol, 0, 199),
  wxSpinCtrl:setToolTip(SpinCtrlCol, "A range from 0 to 200"),
  wxSpinCtrl:connect(SpinCtrlCol,command_spinctrl_updated),

  Options1 = [{border,4}, {flag, ?wxALL}],
  wxSizer:add(SpinSizer, SpinCtrlRow, Options1),
  wxSizer:add(SpinSizer, SpinCtrlCol, Options1),
  MainSizer = wxBoxSizer:new(?wxVERTICAL),
  Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,[]),
  Grid = create_grid(Panel,Size),

  %% Add to sizers
  Options2 = [{flag, ?wxEXPAND}, {proportion, 1}],
  wxSizer:add(Sizer, DataButton, [{border, 5}, {flag, ?wxALL}]),
  wxSizer:add(Sizer, Grid, Options2),
  wxSizer:add(MainSizer, Sizer, Options2),
  wxPanel:setSizerAndFit(Panel,MainSizer),
  wxFrame:connect(Frame,close_window),
  wxFrame:show(Frame),

  {Panel, #state{frame= Frame ,parent=Panel, grid=Grid, x=0,y=0,ets= DisplayEts}}.


handle_call(_Request, _From, State ) ->
  {reply, ok, State}.

%%get message from master, with the new data of the sensor in position {Curr_X,Curr_Y}
handle_cast({{Curr_X,Curr_Y}, {Temp,Wind,Hands,Energy}}, State) ->
  ets:insert(State#state.ets,{{Curr_X,Curr_Y},{Temp,Wind,Hands,Energy}}),
  showDataInGrid(State,{Curr_X,Curr_Y}, {Temp,Wind}),
  {noreply, State };

%%show how much time it took the fire to spread
handle_cast({"Fire Time",Time} ,State) ->
  Text = lists:flatten(io_lib:format("Fire Time ~p seconds", [Time])),
  M = wxMessageDialog:new(State#state.parent,Text),
  wxMessageDialog:showModal(M),
  {noreply, State };

handle_cast(_Request, State) ->
  {noreply, State}.

%%exit button pushed, upadte the master and terminate yourself
handle_event(#wx{event = #wxClose{}},State) ->
  rpc:call(?PC1,masterManager,send,["Exit"]),
  timer:sleep(3000),
  {stop,normal,State};

%%after using the spin, update the State to the new X or Y
handle_event(#wx{id = Id,event = #wxSpin{type = command_spinctrl_updated,commandInt = Int}}, State = #state{}) ->
  case Id of
    1 -> NewX = Int,
      NewY = State#state.y;
    2 -> NewX = State#state.x,
      NewY = Int;
    _ -> NewX = State#state.x,
      NewY = State#state.y
  end,
  {noreply, #state{parent=State#state.parent, grid =State#state.grid, x=NewX,y=NewY, ets = State#state.ets}};

%%after pressing button, go to the wanted operation
handle_event(#wx{id = Id,event = #wxCommand{type = command_button_clicked}},State = #state{}) ->
  case Id of
    1 ->
      new_mini_frame(State);
    2 -> new_dialog(State);
    _ -> ignore
  end,

  {noreply, State};

handle_event(_Request, State) ->
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

%%create the grid SizeXSize
create_grid(Panel,Size) ->
  %% Create the grid with Size * Size cells
  Grid = wxGrid:new(Panel, 2, []),
  wxGrid:createGrid(Grid, Size, Size),
  wxGrid:disableCellEditControl(Grid),
  Mes = (20/Size),

  Fon =
    fun(Col) ->
      T1 = lists:flatten(io_lib:format("~p", [Col+1])),
      wxGrid:setColLabelValue(Grid,Col,T1),
      Forward = fun(Row) -> rowFun(Grid,Row,Col,Mes) end,
      wx:foreach(Forward, lists:seq(0,Size-1))
    end,

  %% Apply the fun to each row
  wx:foreach(Fon, lists:seq(0,Size-1)),
  wxGrid:connect(Grid, grid_cell_change),
  Grid.

%%create the dot in position {Row,Col} with color that match his position
rowFun(Grid,Row,Col,Mes) ->
  Color = get_color(erlang:trunc(Mes*(Row+Col))),
  wxGrid:setReadOnly(Grid, Row, Col, [{isReadOnly,true}]),
  wxGrid:setRowSize(Grid, Row, 15),
  wxGrid:setColSize(Grid, Col, 15),
  wxGrid:setCellBackgroundColour(Grid, Row, Col, Color).

%%after pressing the Data button, create mini frame with the data of the wanted position
new_mini_frame(State) ->
  MiniFrame = wxMiniFrame:new(State#state.parent, ?wxID_ANY, "Sensor Data", [{style,
    ?wxDEFAULT_FRAME_STYLE bor
      ?wxFRAME_FLOAT_ON_PARENT}]),
  Panel = wxPanel:new(MiniFrame, []),
  Q= ets:lookup(State#state.ets,{State#state.x,State#state.y} ),
  if
    Q == [] -> {Temp,Wind,Hands,Energy} = {0,0,0,100};
    true -> [{{_X,_Y},{Temp,Wind,Hands,Energy}}] = Q
  end,
  Text1 = lists:flatten(io_lib:format("Temp Is ~p", [erlang:trunc(Temp)])),
  Text2 = lists:flatten(io_lib:format("Wind Is ~p", [erlang:trunc(Wind)])),
  Text3 = lists:flatten(io_lib:format("Hands Count  ~p", [erlang:trunc(Hands)])),
  Text4 = lists:flatten(io_lib:format("Energy Is ~p%", [erlang:trunc(Energy)])),

  FireButton = wxButton:new(Panel, 2, [{pos, {1, 120}},{label, "Fire?"}]),
  wxMiniFrame:connect(FireButton, command_button_clicked),

  wxStaticText:new(Panel, ?wxID_ANY, Text1, []),
  wxStaticText:new(Panel, ?wxID_ANY, Text2, [{pos, {1, 30}}]),
  wxStaticText:new(Panel, ?wxID_ANY, Text3, [{pos, {1, 60}}]),
  wxStaticText:new(Panel, ?wxID_ANY, Text4, [{pos, {1, 90}}]),
  wxMiniFrame:setSize(MiniFrame, {200,200}),
  wxMiniFrame:center(MiniFrame),
  wxMiniFrame:show(MiniFrame).

%%update the color of the cell to match the new data
showDataInGrid(State,{Curr_X,Curr_Y}, {Temp,_Wind}) ->
  Color = get_color(erlang:trunc(Temp)),
%%  Text = lists:flatten(io_lib:format("{~p,~p}", [erlang:trunc(Temp), erlang:trunc(Wind)])),
  wxGrid:setCellValue(State#state.grid, Curr_X, Curr_Y, " "),
  wxGrid:setCellTextColour(State#state.grid, Curr_X, Curr_Y, ?wxWHITE),
  wxGrid:setCellBackgroundColour(State#state.grid, Curr_X, Curr_Y, Color).

%%get color that match the temp of the cell
get_color(Temp) ->
  if
    Temp<0  ->  Color = ?wxWHITE;
    Temp=<5 -> Color = ?wxBLUE;
    Temp=<10 -> Color = {51,153,255};
    Temp=<15 -> Color = {0,204,204};
    Temp=<20 -> Color = {102,255,178};
    Temp=<25 -> Color = {178,255,102};
    Temp=<30 -> Color = {255,255,51};
    Temp=<35 -> Color = {255,153,51};
    Temp=<45 -> Color = ?wxRED;
    true -> Color = ?wxBLACK
  end,
  Color.

%%send Fire message to the master after Fire button pressed
new_dialog(State)  ->
  rpc:call(?PC1,masterManager,send,[{"Fire!!!" ,{State#state.x,State#state.y,erlang:system_time()}}]).

%%received message from master about an update with sensor {Curr_X,Curr_Y} new data
setData({Curr_X,Curr_Y}, {Temp,Wind,Hands,Energy}) ->
  wx_object:cast(display,{{Curr_X,Curr_Y}, {Temp,Wind,Hands,Energy}});

%%received message from master about the fire that spread, with the time that it took
setData("Fire Time",Time) ->
  wx_object:cast(display,{"Fire Time",Time}).

