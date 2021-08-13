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

-define(SCREEN_SIZE,{1400, 900}).
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
  ets:insert(status_db,[{q1,?INIT_VAL},{q3,?INIT_VAL},{q2,?INIT_VAL},{q4,?INIT_VAL},{whole,?INIT_VAL},{self_temp,0.0}]),

  %Graphics
  WxServer = wx:new(),
  Frame = wxFrame:new(WxServer, ?wxID_ANY, "MAP", [{size,?SCREEN_SIZE}]),
  Panel  = wxPanel:new(Frame),

  wxFrame:show(Frame),
  CallBackPaint =	fun(#wx{event = #wxPaint{}}, _wxObj)->
    Paint = wxBufferedPaintDC:new(Panel),
    BackGround = wxBitmap:new("background.bmp"),
    wxDC:drawBitmap(Paint,BackGround,{0,0}),
    wxBitmap:destroy(BackGround),
    Statistics = wxBitmap:new("statistics.bmp"),
    wxDC:drawBitmap(Paint,Statistics,{735,0}),
    wxBitmap:destroy(Statistics),
    Stationary_comp = wxBitmap:new("stationary_comp1.bmp"),
    wxDC:drawBitmap(Paint,Stationary_comp,{935,5}),
    wxBitmap:destroy(Stationary_comp),
    drawETS(Paint),

    %Status
    Font = wxFont:new(12,?wxDECORATIVE,?wxFONTSTYLE_NORMAL,?wxFONTWEIGHT_LIGHT,[]),
    wxDC:setFont(Paint,Font),
    wxDC:setTextForeground(Paint,?wxBLACK),
    drawTEXT(Paint),
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
      [{Sensor_Pos,SensorIMG}] = ets:lookup(screen_db,Key),
      SensorBMP = wxBitmap:new(SensorIMG),
      wxDC:drawBitmap(Painter, SensorBMP, Sensor_Pos),
      wxBitmap:destroy(SensorBMP),
      drawETS(Painter,ets:next(screen_db,Key),Battery)
  end.

drawTEXT(Painter) ->  drawTEXT(Painter,ets:first(status_db)).
drawTEXT(Painter,Key) ->
  case Key of
    '$end_of_table' -> ok;
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