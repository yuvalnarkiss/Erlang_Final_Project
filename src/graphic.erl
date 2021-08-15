%%%-------------------------------------------------------------------
%%% @author katrinn
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Jul 2021 15:58
%%%-------------------------------------------------------------------
-module(graphic).
-author("katrinn").

-behaviour(wx_object).
-include_lib("wx/include/wx.hrl").
-include("params.hrl").

-define(INIT_VAL,{0.0,0.0}).

%% API
-export([start/0, update_sensor_ets/1, update_battery_ets/1,update_status/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(main_PC_state, {frame, panel, sensor_img}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start() ->
  wx_object:start({local, ?SERVER}, ?MODULE, [], []).

update_sensor_ets(Graphic_ets_list) ->
  wx_object:cast(?SERVER, {update_sensor_ets, Graphic_ets_list}).

update_battery_ets(Graphic_ets_list) ->
  wx_object:cast(?SERVER, {update_battery_ets, Graphic_ets_list}).

update_status(Quarter,Data) ->
  wx_object:cast(?SERVER, {update_stat, {Quarter,Data}}).


%%%===================================================================
%%% wx_object callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
%-spec(init(Args :: term()) ->
%  {ok, State :: #main_PC_state{}} | {ok, State :: #main_PC_state{}, timeout() | hibernate} |
%  {stop, Reason :: term()} | ignore).
init([]) ->
  ets:new(screen_db,[set,public,named_table]),
  ets:new(btry_db,[set,public,named_table]),
  ets:new(status_db,[set,public,named_table]),
  % Initialize Values
  ets:insert(status_db,[{q1,?INIT_VAL},{q3,?INIT_VAL},{q2,?INIT_VAL},{q4,?INIT_VAL},{whole,?INIT_VAL},{self_temp,0.0},{stat_avg,{0.0,0.0,0.0,0.0}}]),

  %Graphics
  WxServer = wx:new(),
  Frame = wxFrame:new(WxServer, ?wxID_ANY, "MAP", [{size,?SCREEN_SIZE}]),
  BackGroundIMG0 = wxImage:new("background.png"),
  BackGroundIMG = wxImage:scale(BackGroundIMG0,?SCREEN_WIDTH,?SCREEN_HEIGHT),
  {SC_X,SC_Y} = {trunc(?STATIONARY_COMP_X/?AREA_WIDTH_METER * ?MAP_WIDTH),trunc(?STATIONARY_COMP_Y/?AREA_HEIGHT_METER * ?MAP_HEIGHT)},
  Panel  = wxPanel:new(Frame),

  %Statistics box-------------------------------------------------------------------
  MainSizer = wxBoxSizer:new(?wxHORIZONTAL),
  Font = wxFont:new(12,?wxDECORATIVE,?wxFONTSTYLE_NORMAL,?wxFONTWEIGHT_LIGHT,[]),

  Title1 = wxStaticText:new(Panel, ?wxID_ANY, "Main Statistics", [{style, ?wxALIGN_CENTER}]),
  wxTextCtrl:setFont(Title1, Font),

  Title2 = wxStaticText:new(Panel, ?wxID_ANY, "Upper Left Quarter Statistics", [{style, ?wxALIGN_CENTER}]),
  wxTextCtrl:setFont(Title2, Font),

  Title3 = wxStaticText:new(Panel, ?wxID_ANY, "Upper Right Quarter Statistics", [{style, ?wxALIGN_CENTER}]),
  wxTextCtrl:setFont(Title3, Font),

  Title4 = wxStaticText:new(Panel, ?wxID_ANY, "Lower Left Quarter Statistics", [{style, ?wxALIGN_CENTER}]),
  wxTextCtrl:setFont(Title4, Font),

  Title5 = wxStaticText:new(Panel, ?wxID_ANY, "Lower Right Quarter Statistics", [{style, ?wxALIGN_CENTER}]),
  wxTextCtrl:setFont(Title5, Font),

  Title6 = wxStaticText:new(Panel, ?wxID_ANY, "Sensor Condition", [{style, ?wxALIGN_CENTER}]),
  wxTextCtrl:setFont(Title6, Font),

  AVG_Temp = wxStaticText:new(Panel, ?wxID_ANY, "Average Temp:", [{style, ?wxALIGN_CENTER}]),
  wxTextCtrl:setFont(AVG_Temp, Font),
  AVG_Temp_TXT1 = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, "***************"}, {style, ?wxTE_RIGHT}]),
  wxTextCtrl:setFont(AVG_Temp_TXT1, Font),
  wxTextCtrl:setEditable(AVG_Temp_TXT1, false),
  AVG_Temp_TXT2 = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, "***************"}, {style, ?wxTE_RIGHT}]),
  wxTextCtrl:setFont(AVG_Temp_TXT2, Font),
  wxTextCtrl:setEditable(AVG_Temp_TXT2, false),
  AVG_Temp_TXT3 = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, "***************"}, {style, ?wxTE_RIGHT}]),
  wxTextCtrl:setFont(AVG_Temp_TXT3, Font),
  wxTextCtrl:setEditable(AVG_Temp_TXT3, false),
  AVG_Temp_TXT4 = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, "***************"}, {style, ?wxTE_RIGHT}]),
  wxTextCtrl:setFont(AVG_Temp_TXT4, Font),
  wxTextCtrl:setEditable(AVG_Temp_TXT4, false),
  AVG_Temp_TXT5 = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, "***************"}, {style, ?wxTE_RIGHT}]),
  wxTextCtrl:setFont(AVG_Temp_TXT5, Font),
  wxTextCtrl:setEditable(AVG_Temp_TXT5, false),

  AVG_Hum = wxStaticText:new(Panel, ?wxID_ANY, "Average Humidity:", [{style, ?wxALIGN_CENTER}]),
  wxTextCtrl:setFont(AVG_Hum, Font),
  AVG_Hum_TXT1 = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, "***************"}, {style, ?wxTE_RIGHT}]),
  wxTextCtrl:setFont(AVG_Hum_TXT1, Font),
  wxTextCtrl:setEditable(AVG_Hum_TXT1, false),
  AVG_Hum_TXT2 = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, "***************"}, {style, ?wxTE_RIGHT}]),
  wxTextCtrl:setFont(AVG_Hum_TXT2, Font),
  wxTextCtrl:setEditable(AVG_Hum_TXT2, false),
  AVG_Hum_TXT3 = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, "***************"}, {style, ?wxTE_RIGHT}]),
  wxTextCtrl:setFont(AVG_Hum_TXT3, Font),
  wxTextCtrl:setEditable(AVG_Hum_TXT3, false),
  AVG_Hum_TXT4 = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, "***************"}, {style, ?wxTE_RIGHT}]),
  wxTextCtrl:setFont(AVG_Hum_TXT4, Font),
  wxTextCtrl:setEditable(AVG_Hum_TXT4, false),
  AVG_Hum_TXT5 = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, "***************"}, {style, ?wxTE_RIGHT}]),
  wxTextCtrl:setFont(AVG_Hum_TXT5, Font),
  wxTextCtrl:setEditable(AVG_Hum_TXT5, false),


  Sens_Temp = wxStaticText:new(Panel, ?wxID_ANY, "Average Sensor's Temp:", [{style, ?wxALIGN_CENTER}]),
  wxTextCtrl:setFont(Sens_Temp, Font),
  Sens_Temp_TXT = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, "***************"}, {style, ?wxTE_RIGHT}]),
  wxTextCtrl:setFont(Sens_Temp_TXT, Font),
  wxTextCtrl:setEditable(Sens_Temp_TXT, false),

  Btry_lvl = wxStaticText:new(Panel, ?wxID_ANY, "Average Battery Level:", [{style, ?wxALIGN_CENTER}]),
  wxTextCtrl:setFont(Btry_lvl, Font),
  Btry_lvl_TXT = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, "***************"}, {style, ?wxTE_RIGHT}]),
  wxTextCtrl:setFont(Btry_lvl_TXT, Font),
  wxTextCtrl:setEditable(Btry_lvl_TXT, false),

  Btry_SD = wxStaticText:new(Panel, ?wxID_ANY, "Battery Standard Devition:", [{style, ?wxALIGN_CENTER}]),
  wxTextCtrl:setFont(Btry_SD, Font),
  Btry_SD_TXT = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, "***************"}, {style, ?wxTE_RIGHT}]),
  wxTextCtrl:setFont(Btry_SD_TXT, Font),
  wxTextCtrl:setEditable(Btry_SD_TXT, false),

  AVG_MSG = wxStaticText:new(Panel, ?wxID_ANY, "Average No. of messages:", [{style, ?wxALIGN_CENTER}]),
  wxTextCtrl:setFont(AVG_MSG, Font),
  AVG_MSG_TXT = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, "***************"}, {style, ?wxTE_RIGHT}]),
  wxTextCtrl:setFont(AVG_MSG_TXT, Font),
  wxTextCtrl:setEditable(AVG_MSG_TXT, false),

  MSG_SD = wxStaticText:new(Panel, ?wxID_ANY, "No. of sent messages~nStandard Devition:", [{style, ?wxALIGN_CENTER}]),
  wxTextCtrl:setFont(MSG_SD, Font),
  MSG_SD_TXT = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, "***************"}, {style, ?wxTE_RIGHT}]),
  wxTextCtrl:setFont(MSG_SD_TXT, Font),
  wxTextCtrl:setEditable(MSG_SD_TXT, false),

  LineAVGTemp1 = wxBoxSizer:new(?wxHORIZONTAL),
  wxSizer:add(LineAVGTemp1, AVG_Temp, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  wxSizer:add(LineAVGTemp1, AVG_Temp_TXT1,    [{proportion,7},{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),

  %LineAVGTemp2 = wxBoxSizer:new(?wxHORIZONTAL),
  %wxSizer:add(LineAVGTemp2, AVG_Temp, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  %wxSizer:add(LineAVGTemp2, AVG_Temp_TXT2,    [{proportion,7},{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),

  %LineAVGTemp3 = wxBoxSizer:new(?wxHORIZONTAL),
  %wxSizer:add(LineAVGTemp3, AVG_Temp, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  %wxSizer:add(LineAVGTemp3, AVG_Temp_TXT3,    [{proportion,7},{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),

  %LineAVGTemp4 = wxBoxSizer:new(?wxHORIZONTAL),
  %wxSizer:add(LineAVGTemp4, AVG_Temp, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  %wxSizer:add(LineAVGTemp4, AVG_Temp_TXT4,    [{proportion,7},{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),

  %LineAVGTemp5 = wxBoxSizer:new(?wxHORIZONTAL),
  %wxSizer:add(LineAVGTemp5, AVG_Temp, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  %wxSizer:add(LineAVGTemp5, AVG_Temp_TXT5,    [{proportion,7},{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),

  LineAVGHum1 = wxBoxSizer:new(?wxHORIZONTAL),
  wxSizer:add(LineAVGHum1, AVG_Hum, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  wxSizer:add(LineAVGHum1, AVG_Hum_TXT1,    [{proportion,7},{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),

  %LineAVGHum2 = wxBoxSizer:new(?wxHORIZONTAL),
  %wxSizer:add(LineAVGHum2, AVG_Hum, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  %wxSizer:add(LineAVGHum2, AVG_Hum_TXT2,    [{proportion,7},{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),

  %LineAVGHum3 = wxBoxSizer:new(?wxHORIZONTAL),
  %wxSizer:add(LineAVGHum3, AVG_Hum, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  %wxSizer:add(LineAVGHum3, AVG_Hum_TXT3,    [{proportion,7},{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),

  %LineAVGHum4 = wxBoxSizer:new(?wxHORIZONTAL),
  %wxSizer:add(LineAVGHum4, AVG_Hum, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  %wxSizer:add(LineAVGHum4, AVG_Hum_TXT4,    [{proportion,7},{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),

  %LineAVGHum5 = wxBoxSizer:new(?wxHORIZONTAL),
  %wxSizer:add(LineAVGHum5, AVG_Hum, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  %wxSizer:add(LineAVGHum5, AVG_Hum_TXT5,    [{proportion,7},{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),


  LineAVGSensTemp = wxBoxSizer:new(?wxHORIZONTAL),
  wxSizer:add(LineAVGSensTemp, Sens_Temp, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  wxSizer:add(LineAVGSensTemp, Sens_Temp_TXT,    [{proportion,7},{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),

  LineAVGBtry = wxBoxSizer:new(?wxHORIZONTAL),
  wxSizer:add(LineAVGBtry, Btry_lvl, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  wxSizer:add(LineAVGBtry, Btry_lvl_TXT,    [{proportion,7},{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),

  LineBtrySD = wxBoxSizer:new(?wxHORIZONTAL),
  wxSizer:add(LineBtrySD, Btry_SD, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  wxSizer:add(LineBtrySD, Btry_SD_TXT,    [{proportion,7},{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),

  LineAVGMsg = wxBoxSizer:new(?wxHORIZONTAL),
  wxSizer:add(LineAVGMsg, AVG_MSG, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  wxSizer:add(LineAVGMsg, AVG_MSG_TXT,    [{proportion,7},{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),

  LineMsgSD = wxBoxSizer:new(?wxHORIZONTAL),
  wxSizer:add(LineMsgSD, MSG_SD, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  wxSizer:add(LineMsgSD, MSG_SD_TXT,    [{proportion,7},{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),

  StatisticsSizer = wxBoxSizer:new(?wxVERTICAL),
  wxSizer:add(StatisticsSizer, Title1, [{flag, ?wxEXPAND}]),
  wxSizer:add(StatisticsSizer, LineAVGTemp1, [{flag, ?wxEXPAND}]),
  wxSizer:add(StatisticsSizer, LineAVGHum1, [{flag, ?wxEXPAND}]),
  wxSizer:add(StatisticsSizer, Title2, [{flag, ?wxEXPAND}]),
  %wxSizer:add(StatisticsSizer, LineAVGTemp2, [{flag, ?wxEXPAND}]),
  %wxSizer:add(StatisticsSizer, LineAVGHum2, [{flag, ?wxEXPAND}]),
  wxSizer:add(StatisticsSizer, Title3, [{flag, ?wxEXPAND}]),
  %wxSizer:add(StatisticsSizer, LineAVGTemp3, [{flag, ?wxEXPAND}]),
  %wxSizer:add(StatisticsSizer, LineAVGHum3, [{flag, ?wxEXPAND}]),
  wxSizer:add(StatisticsSizer, Title4, [{flag, ?wxEXPAND}]),
  %wxSizer:add(StatisticsSizer, LineAVGTemp4, [{flag, ?wxEXPAND}]),
  %wxSizer:add(StatisticsSizer, LineAVGHum4, [{flag, ?wxEXPAND}]),
  wxSizer:add(StatisticsSizer, Title5, [{flag, ?wxEXPAND}]),
  %wxSizer:add(StatisticsSizer, LineAVGTemp5, [{flag, ?wxEXPAND}]),
  %wxSizer:add(StatisticsSizer, LineAVGHum5, [{flag, ?wxEXPAND}]),
  wxSizer:add(StatisticsSizer, Title6, [{flag, ?wxEXPAND}]),
  wxSizer:add(StatisticsSizer, LineAVGSensTemp, [{flag, ?wxEXPAND}]),
  wxSizer:add(StatisticsSizer, LineAVGBtry, [{flag, ?wxEXPAND}]),
  wxSizer:add(StatisticsSizer, LineBtrySD, [{flag, ?wxEXPAND}]),
  wxSizer:add(StatisticsSizer, LineAVGMsg, [{flag, ?wxEXPAND}]),
  wxSizer:add(StatisticsSizer, LineMsgSD, [{flag, ?wxEXPAND}]),

  wxSizer:add(MainSizer, StatisticsSizer,    [{proportion,1},{flag, ?wxALIGN_RIGHT}, {border, 5}]),

  wxWindow:setSizer(Frame, MainSizer),

  Vbox = wxBoxSizer:new(?wxVERTICAL),
  wxSizer:add(Vbox, Panel, [{flag, ?wxEXPAND}]),

  wxFrame:show(Frame),
  CallBackPaint =	fun(#wx{event = #wxPaint{}}, _wxObj)->
    Paint = wxBufferedPaintDC:new(Panel),
    BackGround = wxBitmap:new(BackGroundIMG),
    wxDC:drawBitmap(Paint,BackGround,{0,0}),
    wxBitmap:destroy(BackGround),
    %Statistics = wxBitmap:new("statistics.bmp"),
    %wxDC:drawBitmap(Paint,Statistics,{735,0}),
    %wxBitmap:destroy(Statistics),
    Stationary_comp = wxBitmap:new("stationary_comp1.bmp"),
    wxDC:drawBitmap(Paint,Stationary_comp,{SC_X,SC_Y}),
    wxBitmap:destroy(Stationary_comp),
    drawETS(Paint),

    [{_Key, {BatteryAVG,BatterySTD,MsgCountAVG,MsgCountSTD}}] = ets:lookup(status_db,stat_avg),
    [{_Key1, SelfTemp}] = ets:lookup(status_db,self_temp),
    [{_Key2, {TempAVG1,HumidityAVG1}}] = ets:lookup(status_db,whole),
    [{_Key3, {TempAVG2,HumidityAVG2}}] = ets:lookup(status_db,q1),
    [{_Key4, {TempAVG3,HumidityAVG3}}] = ets:lookup(status_db,q2),
    [{_Key5, {TempAVG4,HumidityAVG4}}] = ets:lookup(status_db,q3),
    [{_Key6, {TempAVG5,HumidityAVG5}}] = ets:lookup(status_db,q4),
    %Status
    wxTextCtrl:changeValue(Btry_lvl_TXT, lists:flatten(io_lib:format("~.3f",[BatteryAVG]))),
    wxTextCtrl:changeValue(Btry_SD_TXT, lists:flatten(io_lib:format("~.3f",[BatterySTD]))),
    wxTextCtrl:changeValue(AVG_MSG_TXT, lists:flatten(io_lib:format("~.3f",[MsgCountAVG]))),
    wxTextCtrl:changeValue(MSG_SD_TXT, lists:flatten(io_lib:format("~.3f",[MsgCountSTD]))),
    wxTextCtrl:changeValue(Sens_Temp_TXT, lists:flatten(io_lib:format("~.3f",[SelfTemp]))),

    wxTextCtrl:changeValue(AVG_Temp_TXT1, lists:flatten(io_lib:format("~.3f",[TempAVG1]))),
    wxTextCtrl:changeValue(AVG_Hum_TXT1, lists:flatten(io_lib:format("~.3f",[HumidityAVG1]))),
    wxTextCtrl:changeValue(AVG_Temp_TXT2, lists:flatten(io_lib:format("~.3f",[TempAVG2]))),
    wxTextCtrl:changeValue(AVG_Hum_TXT2, lists:flatten(io_lib:format("~.3f",[HumidityAVG2]))),
    wxTextCtrl:changeValue(AVG_Temp_TXT3, lists:flatten(io_lib:format("~.3f",[TempAVG3]))),
    wxTextCtrl:changeValue(AVG_Hum_TXT3, lists:flatten(io_lib:format("~.3f",[HumidityAVG3]))),
    wxTextCtrl:changeValue(AVG_Temp_TXT4, lists:flatten(io_lib:format("~.3f",[TempAVG4]))),
    wxTextCtrl:changeValue(AVG_Hum_TXT4, lists:flatten(io_lib:format("~.3f",[HumidityAVG4]))),
    wxTextCtrl:changeValue(AVG_Temp_TXT5, lists:flatten(io_lib:format("~.3f",[TempAVG5]))),
    wxTextCtrl:changeValue(AVG_Hum_TXT5, lists:flatten(io_lib:format("~.3f",[HumidityAVG5]))),




    wxPanel:refresh(Btry_lvl_TXT),
    wxPanel:refresh(Btry_SD_TXT),
    wxPanel:refresh(AVG_MSG_TXT),
    wxPanel:refresh(MSG_SD_TXT),
    wxPanel:refresh(Sens_Temp_TXT),
    wxPanel:refresh(AVG_Temp_TXT1),
    wxPanel:refresh(AVG_Hum_TXT1),
    wxPanel:refresh(AVG_Temp_TXT2),
    wxPanel:refresh(AVG_Hum_TXT2),
    wxPanel:refresh(AVG_Temp_TXT3),
    wxPanel:refresh(AVG_Hum_TXT3),
    wxPanel:refresh(AVG_Temp_TXT4),
    wxPanel:refresh(AVG_Hum_TXT4),
    wxPanel:refresh(AVG_Temp_TXT5),
    wxPanel:refresh(AVG_Hum_TXT5),
    wxBufferedPaintDC:destroy(Paint) end,

  % connect panel
  wxFrame:connect(Panel, paint, [{callback, CallBackPaint}]),
  % connect mouse
  wxPanel:connect (Panel, left_down),

  % Self messaging timer
  timer:send_interval(80, self(), graphicUpdate),

  %sensor:start(sensor1), %ToDo: temp
  {Frame, #main_PC_state{frame = Frame, panel = Panel}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #main_PC_state{}) ->
  {reply, Reply :: term(), NewState :: #main_PC_state{}} |
  {reply, Reply :: term(), NewState :: #main_PC_state{}, timeout() | hibernate} |
  {noreply, NewState :: #main_PC_state{}} |
  {noreply, NewState :: #main_PC_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #main_PC_state{}} |
  {stop, Reason :: term(), NewState :: #main_PC_state{}}).
handle_call(_Request, _From, State = #main_PC_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #main_PC_state{}) ->
  {noreply, NewState :: #main_PC_state{}} |
  {noreply, NewState :: #main_PC_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #main_PC_state{}}).
handle_cast({update_sensor_ets, Graphic_ets_list}, State) ->
  Draw_List = [{Sensor_Pos,state_to_img(Sensor_State)} || {Sensor_Pos,Sensor_State} <- Graphic_ets_list],
  ets:insert(screen_db,Draw_List),
  {noreply, State};
handle_cast({update_battery_ets, Graphic_ets_list}, State) ->
  Draw_List = [{Battery_Pos,b_state_to_img(Battery_State)} || {Battery_Pos,Battery_State} <- Graphic_ets_list, Battery_State /= 0],
  ets:insert(btry_db,Draw_List),

  %delete images with 0 battery
  [ ets:delete(btry_db,Battery_Pos) || {Battery_Pos,Battery_State} <- Graphic_ets_list, Battery_State == 0],
  {noreply, State};
handle_cast({update_stat, {Quarter,Data}}, State) ->
  ets:insert(status_db,{Quarter,Data}), %{TempAVG,SelfTempAVG,HumidityAVG}
  {noreply, State};
handle_cast(_Request, State = #main_PC_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #main_PC_state{}) ->
  {noreply, NewState :: #main_PC_state{}} |
  {noreply, NewState :: #main_PC_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #main_PC_state{}}).
handle_info(_Info, State = #main_PC_state{ panel = Panel }) ->
  wxWindow:refresh(Panel,[{eraseBackground,false}]),
  {noreply, State}.

handle_event(#wx{event = #wxMouse{type=left_down, x=X0, y=Y0}},State) -> % when the right click was pressed, activate a function that returns the color of the light that was pressed
  X = trunc(X0/20) * 20,
  Y = trunc(Y0/20) * 20,
  Battery_state = ets:lookup(btry_db,{X,Y}),
  case Battery_state of
    [] -> ok;
    [{_Battery_Pos,BatteryIMG}] ->
      ets:insert(screen_db,{battery,{{X-5,Y-20},BatteryIMG}}),
      timer:apply_after(1000,ets,delete,[screen_db, battery])   % show for 1 sec
  end,
  {noreply,State};

handle_event(#wx{event = #wxClose{}},State = #main_PC_state {frame = Frame, panel = Panel}) -> % close window event
  io:format("Exiting\n"),
  wxPanel:destroy(Panel),
  wxWindow:destroy(Frame),
  wx:destroy(),
  {stop,normal,State};

handle_event(_Event,State) ->
  {noreply,State}.


%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #main_PC_state{}) -> term()).
terminate(_Reason, _State = #main_PC_state{}) ->
  ets:delete(screen_db), ets:delete(btry_db), ets:delete(status_db),
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #main_PC_state{},
    Extra :: term()) ->
  {ok, NewState :: #main_PC_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #main_PC_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
% ===== iterates all ets elements and update image =====
drawETS(Painter) -> drawETS(Painter,ets:first(screen_db),none).
drawETS(Painter,Key,Battery) ->
  case Key of
    '$end_of_table' ->
      case Battery of
        none -> ok;
        {Battery_Pos,BatteryIMG} ->
          BatteryBMP = wxBitmap:new(BatteryIMG),
          wxDC:drawBitmap(Painter, BatteryBMP, Battery_Pos),
          wxBitmap:destroy(BatteryBMP)
      end;
    battery ->
      [{_Battery,Battery_State}] = ets:lookup(screen_db,Key),
      drawETS(Painter,ets:next(screen_db,Key),Battery_State);
    _ ->
      [{{Sensor_X,Sensor_Y},SensorIMG}] = ets:lookup(screen_db,Key),
      Pixel_Pos =  {trunc(Sensor_X/?AREA_WIDTH_METER * ?MAP_WIDTH),trunc(Sensor_Y/?AREA_HEIGHT_METER * ?MAP_HEIGHT)},
      SensorBMP = wxBitmap:new(SensorIMG),
      wxDC:drawBitmap(Painter, SensorBMP, Pixel_Pos),
      wxBitmap:destroy(SensorBMP),
      drawETS(Painter,ets:next(screen_db,Key),Battery)
  end.

drawTEXT(Painter) ->  drawTEXT(Painter,ets:first(status_db)).
drawTEXT(Painter,Key) ->
  case Key of
    '$end_of_table' -> ok;
    stat_avg ->
      [{_Key, {{BatteryAVG,BatterySTD},{MsgCountAVG,MsgCountSTD}}}] = ets:lookup(status_db,Key),
      wxDC:drawText(Painter,io_lib:format("~.3f",[BatteryAVG]) ,{1250,669}),
      wxDC:drawText(Painter,io_lib:format("~.3f",[BatterySTD]) ,{1250,670}),
      wxDC:drawText(Painter,io_lib:format("~.3f",[MsgCountAVG]) ,{1250,671}),
      wxDC:drawText(Painter,io_lib:format("~.3f",[MsgCountSTD]) ,{1250,672}),
      drawTEXT(Painter,ets:next(status_db,Key));
    self_temp ->
      [{_Key,SelfTemp}] = ets:lookup(status_db,Key),
      wxDC:drawText(Painter,io_lib:format("~.3f",[SelfTemp]) ++ "  Celcius",{1250,765}),
      drawTEXT(Painter,ets:next(status_db,Key));
    Key ->
      [{_Key,{TempAVG,HumidityAVG}}] = ets:lookup(status_db,Key),
      {Pos1,Pos2} = case Key of
                      whole -> {{1250,75},{1250,116}};
                      q1 -> {{1250,213},{1250,254}};
                      q2 -> {{1250,351},{1250,392}};
                      q3 -> {{1250,489},{1250,530}};
                      q4 -> {{1250,627},{1250,668}}
                    end,
      wxDC:drawText(Painter,io_lib:format("~.3f",[TempAVG]) ++ "  Celcius",Pos1),
      wxDC:drawText(Painter,io_lib:format("~.3f",[HumidityAVG]) ++ "%",Pos2),
      drawTEXT(Painter,ets:next(status_db,Key))
  end.

state_to_img(State) ->
  case State of
    active -> "active_sensor.bmp";
    sending -> "sensor_sending_data.bmp";
    asleep -> "sensor_sleep.bmp";
    inactive -> "sensor_inactive.bmp"
  end.

b_state_to_img(State) ->
  case State of
    low_battery -> "battery_15.bmp";
    20 -> "battery_20.bmp";
    40 -> "battery_40.bmp";
    60 -> "battery_60.bmp";
    80 -> "battery_80.bmp";
    100 -> "battery_100.bmp"
  end.