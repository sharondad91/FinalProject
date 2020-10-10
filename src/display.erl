%%%-------------------------------------------------------------------
%%% @author sharondad
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(display).

-behaviour(wx_object).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2,handle_event/2, handle_info/2, terminate/2,
  code_change/3,test/2]).

-include_lib("wx/include/wx.hrl").

-define(SERVER, ?MODULE).

-define(MapSize, 500).
-define(max_x,(1000)).
-define(max_y,(1000)).

-record(state,
{
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

start_link() ->
  %%  wx_object:start_link({local,?SERVER},?MODULE,[global,Node],[]).
  wx_object:start_link({local,?SERVER},?MODULE,[],[]).

init([]) ->


%%  DisplayEts = ets:new(displayets,[set,named_table,public]),
  DisplayEts = ets:new(displayets,[set]),

%%  Wx = wx:new(),
%%%%  Frame = wxFrame:new(wx:null(),-1,"Orshi and Sharon"),
%%  Frame = wxFrame:new(Wx, -1, "Concurent and Distributed LAB", [{size, {?max_x, ?max_y}}]),
%%  MainSizer = wxBoxSizer:new(?wxHORIZONTAL),
%%%%%%  ControlSizer = wxBoxSizer:new(?wxVERTICAL),
%%%%%%  Size = ?MapSize,
%%%%%%  Panel = wxPanel:new(Frame,[{size,{Size,Size}},{style,?wxFULL_REPAINT_ON_RESIZE}]),
%%
%%  %% maps chooser
%%  MapsLabel = wxStaticText:new(Frame,?wxID_ANY,"Map",[{style,?wxALIGN_LEFT}]),
%%  wxStaticText:wrap(MapsLabel,100),
%%  Choices = ["","Borders","Middle","Gap"],
%%  MapChooser = wxComboBox:new(Frame, length(Choices), [{choices, Choices}]),
%%  wxComboBox:setToolTip(MapChooser, "Room Chooser"),
%%
%%  wxWindow:setSizer(Frame, MainSizer),
%%  wxWindow:setMinSize(Frame,wxWindow:getSize(Frame)),
%%
%%  wxGrid:new(),
%%
%%%%  wxWindow:setSizer(Frame, MainSizer),
%%
%%  wxFrame:connect (Frame, command_menu_selected),
%%  wxFrame:show(Frame),

  %% Create Board
  WxServer = wx:new(),
  Frame = wxFrame:new(WxServer, ?wxID_ANY, "PLAY", [{size,{?max_x, ?max_y}}]),
  Panel  = wxPanel:new(Frame),

  %% Create Buttons
  DataButton = wxButton:new(Panel, 1, [{label, "Data"}]),
  wxPanel:connect(DataButton, command_button_clicked),
%%  FireButton = wxButton:new(Panel, 2, [{label, "Fire"}]),
%%  wxPanel:connect(FireButton, command_button_clicked),


  %% Create a wxSpinCtrl with range between 0 and 100
  SpinSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,[]),
  SpinCtrlRow = wxSpinCtrl:new(Panel, [{id, 1},{pos, {100,20}},{size, {70,30}}]),
  wxSpinCtrl:setRange(SpinCtrlRow, 0, 199),
  wxSpinCtrl:setToolTip(SpinCtrlRow, "A wxSpinCtrl with range from 0 to 200"),
  wxSpinCtrl:connect(SpinCtrlRow,command_spinctrl_updated),
  SpinCtrlCol = wxSpinCtrl:new(Panel, [{id, 2}, {pos, {170,20}},{size, {70,30}}]),
  wxSpinCtrl:setRange(SpinCtrlCol, 0, 199),
  wxSpinCtrl:setToolTip(SpinCtrlCol, "A wxSpinCtrl with range from 0 to 200"),
  wxSpinCtrl:connect(SpinCtrlCol,command_spinctrl_updated),

  Options1 = [{border,4}, {flag, ?wxALL}],
  wxSizer:add(SpinSizer, SpinCtrlRow, Options1),
  wxSizer:add(SpinSizer, SpinCtrlCol, Options1),
  MainSizer = wxBoxSizer:new(?wxVERTICAL),
  Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,[]),
  Grid = create_grid(Panel),
  %% Add to sizers
  Options2 = [{flag, ?wxEXPAND}, {proportion, 1}],
  wxSizer:add(Sizer, DataButton, [{border, 5}, {flag, ?wxALL}]),
%%  wxSizer:add(Sizer, FireButton, [{border, 5}, {flag, ?wxALL}]),
  wxSizer:add(Sizer, Grid, Options2),
  wxSizer:add(MainSizer, Sizer, Options2),
  wxPanel:setSizer(Panel, MainSizer),
  wxFrame:show(Frame),


  spawn_link(fun() -> spam_Messages() end),

  {Panel, #state{parent=Panel, grid=Grid, x=0,y=0,ets= DisplayEts}}.
%%  {Frame,ok}.
handle_call(_Request, _From, State ) ->
  {reply, ok, State}.

handle_cast({{Curr_X,Curr_Y}, {Temp,Wind}}, State) ->
  ets:insert(State#state.ets,{{Curr_X,Curr_Y},{Temp,Wind}}),
  showDataInGrid(State,{Curr_X,Curr_Y}, {Temp,Wind}),
  {noreply, State };

handle_cast(_Request, State) ->
  {noreply, State}.

handle_event(#wx{id = Id,event = #wxSpin{type = command_spinctrl_updated,commandInt = Int}}, State = #state{}) ->
  case Id of
    1 -> NewX = Int,
          NewY = State#state.y;
    2 -> NewX = State#state.x,
          NewY = Int;
    _ -> NewX = State#state.x,
      NewY = State#state.y
  end,
%%  demo:format(State#state.config,"wxSpinCtrl changed to ~p\n",[Int]),
  {noreply, #state{parent=State#state.parent, grid =State#state.grid, x=NewX,y=NewY, ets = State#state.ets}};

handle_event(#wx{id = Id,event = #wxCommand{type = command_button_clicked}},State = #state{}) ->
  case Id of
    1 ->
%%      showDataInGrid(State);
      new_mini_frame(State);
    2 -> new_dialog(State#state.parent);
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

create_grid(Panel) ->
  %% Create the grid with 200 * 200 cells
  Grid = wxGrid:new(Panel, 2, []),
  wxGrid:createGrid(Grid, 200, 200),
%%  wxGrid:autoSizeRows(Grid,[{'setAsMin', true}]),
%%  wxGrid:autoSizeColumns(Grid,[{'setAsMin', true}]),



%%  Font = wxFont:new(12, ?wxFONTFAMILY_SWISS,
%%    ?wxFONTSTYLE_NORMAL,
%%    ?wxFONTWEIGHT_NORMAL, []),
%%
%%
%%%%   Fun to set the values and flags of the cells
%%%%  Fun =
%%%%    fun(Row) ->
%%%%      Col = 0,
%%%%
%%%%      wxGrid:setCellValue(Grid, Row, Col, "Editable"),
%%%%      wxGrid:setCellValue(Grid, Row, 1, "sHARON"),
%%%%      wxGrid:setCellValue(Grid, Row, 2, "oRSHI"),
%%
%%  Fon =
%%    fun(Col) ->
%%      Forward = fun(Row) -> rowFun(Grid,Row,Col) end,
%%      wx:foreach(Forward, lists:seq(0,199))
%%      end,
%%
%%  %% Apply the fun to each row
%%  wx:foreach(Fon, lists:seq(0,199)),
  wxGrid:connect(Grid, grid_cell_change),
  Grid.

rowFun(Grid,Row,Col) ->
%%  wxGrid:setCellTextColour(Grid, Row, 3, ?wxWHITE),
  wxGrid:setReadOnly(Grid, Row, Col, [{isReadOnly,true}]),
  wxGrid:setRowSize(Grid, Row, 15),
  wxGrid:setColSize(Grid, Col, 15).

%%  case Row rem 2 of
%%    0 -> wxGrid:setCellBackgroundColour(Grid, Row, Col, ?wxRED),
%%      wxGrid:setCellTextColour(Grid, Row, Col, {200,25,150,255});
%%    1 -> wxGrid:setCellBackgroundColour(Grid, Row, Col, ?wxGREEN),
%%      wxGrid:setCellTextColour(Grid, Row, Col, {255,215,0,255}),
%%      wxGrid:setCellAlignment(Grid, Row, Col,
%%        0,?wxALIGN_CENTER),
%%      wxGrid:setCellAlignment(Grid, Row, Col,
%%        ?wxALIGN_CENTER,0),
%%      wxGrid:setCellTextColour(Grid, Row, Col, ?wxBLACK),
%%      wxGrid:setCellAlignment(Grid, Row, Col,
%%        ?wxALIGN_CENTER,
%%        ?wxALIGN_CENTER)
%%%%      wxGrid:setCellFont(Grid, Row, 0, Font),
%%
%%  end.


new_mini_frame(State) ->
  MiniFrame = wxMiniFrame:new(State#state.parent, ?wxID_ANY, "Sensor Data", [{style,
    ?wxDEFAULT_FRAME_STYLE bor
      ?wxFRAME_FLOAT_ON_PARENT}]),
  Panel = wxPanel:new(MiniFrame, []),
  Q= ets:lookup(State#state.ets,{State#state.x,State#state.y} ),
  if
    Q == [] -> {Temp,Wind} = {0,0};
    true -> [{Temp,Wind}] = Q
  end,
%%  {Temp,Wind} = get({State#state.x, State#state.y}),
  Text1 = lists:flatten(io_lib:format("Temp Is ~p", [Temp])),
  Text2 = lists:flatten(io_lib:format("Wind Is ~p", [Wind])),

  DataButton = wxButton:new(Panel, 2, [{pos, {1, 70}},{label, "Fire?"}]),
  wxMiniFrame:connect(DataButton, command_button_clicked),



  wxStaticText:new(Panel, ?wxID_ANY, Text1, []),
  wxStaticText:new(Panel, ?wxID_ANY, Text2, [{pos, {1, 30}}]),
  wxMiniFrame:setSize(MiniFrame, {200,200}),
  wxMiniFrame:center(MiniFrame),
  wxMiniFrame:show(MiniFrame).

showDataInGrid(State,{Curr_X,Curr_Y}, {Temp,Wind}) ->
  Color = get_color(erlang:trunc(Temp)),
  Text = lists:flatten(io_lib:format("{~p,~p}", [erlang:trunc(Temp), erlang:trunc(Wind)])),
  wxGrid:setCellValue(State#state.grid, Curr_X, Curr_Y, Text),
  wxGrid:setCellTextColour(State#state.grid, Curr_X, Curr_Y, ?wxWHITE),
  wxGrid:setCellBackgroundColour(State#state.grid, Curr_X, Curr_Y, Color).


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
    Temp=<40 -> Color = ?wxRED;
    true -> Color = ?wxBLACK
  end,
  Color.


new_dialog(Parent)  ->
  M = wxMessageDialog:new(Parent, "Run For Your Life!!!!!"),
  wxMessageDialog:showModal(M).

test({Curr_X,Curr_Y}, {Temp,Wind}) ->
  wx_object:cast(display,{{Curr_X,Curr_Y}, {Temp,Wind}}).


spam_Messages() ->
  wx_object:cast(display,{{rand:uniform(40),rand:uniform(40)}, {rand:uniform(40),rand:uniform(30)}}),

  spam_Messages().