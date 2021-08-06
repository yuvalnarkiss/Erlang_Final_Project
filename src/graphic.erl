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

%% API
-export([start/0, update_sensor/1, update_battery/1]).

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

update_sensor({_Pos, inactive}) ->
  wx_object:call(?SERVER, {update_img, {_Pos, inactive}});
update_sensor(Sensor_State) ->
  wx_object:cast(?SERVER, {update_img, Sensor_State}).

update_battery(Battery_State) ->
  wx_object:cast(?SERVER, {update_btry, Battery_State}).  %{Battery_Pos,Battery_State} = Battery_State


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
  %ets:insert(sensor_states,[]),
  %Graphics
  WxServer = wx:new(),
  Frame = wxFrame:new(WxServer, ?wxID_ANY, "MAP", [{size,{980, 980}}]),
  Panel  = wxPanel:new(Frame),
  %wxFrame:createStatusBar(Frame), - in case we'll need
  wxFrame:show(Frame),
  BackGround = wxBitmap:new("background.bmp"),
  Stationary_comp = wxBitmap:new("stationary_comp1.bmp"),
  CallBackPaint =	fun(#wx{event = #wxPaint{}}, _wxObj)->
    Paint = wxBufferedPaintDC:new(Panel),
    wxDC:drawBitmap(Paint,BackGround,{0,0}),
    wxDC:drawBitmap(Paint,Stationary_comp,{935,5}),
    %wxBitmap:destroy(BackGround),
    drawETS(Paint),
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
handle_call({update_img, {Sensor_Pos,_Sensor_State}}, _From, State) ->
  case ets:member(screen_db,Sensor_Pos) of
    true ->
      [{Sensor_Pos,SensorBMP_prev}] = ets:lookup(screen_db,Sensor_Pos),
      wxBitmap:destroy(SensorBMP_prev);
    false -> ok
  end,
  SensorBMP = wxBitmap:new("sensor_inactive.bmp"),
  ets:insert(screen_db,{Sensor_Pos,SensorBMP}),
  {reply, ok, State};
handle_call(_Request, _From, State = #main_PC_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #main_PC_state{}) ->
  {noreply, NewState :: #main_PC_state{}} |
  {noreply, NewState :: #main_PC_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #main_PC_state{}}).
handle_cast({update_img, {Sensor_Pos,Sensor_State}}, State) ->
  State_img = case Sensor_State of
    active -> "active_sensor.bmp";
    sending -> "sensor_sending_data.bmp";
    asleep -> "sensor_sleep.bmp"
  end,
  case ets:member(screen_db,Sensor_Pos) of
    true ->
      [{Sensor_Pos,SensorBMP_prev}] = ets:lookup(screen_db,Sensor_Pos),
      wxBitmap:destroy(SensorBMP_prev);
    false -> ok
  end,
  SensorBMP = wxBitmap:new(State_img),
  ets:insert(screen_db,{Sensor_Pos,SensorBMP}),
  {noreply, State};
handle_cast({update_btry, {Battery_Pos,Battery_State}}, State) ->
  State_img = case Battery_State of
                low_battery -> "battery_15.bmp";
                20 -> "battery_20.bmp";
                40 -> "battery_40.bmp";
                60 -> "battery_60.bmp";
                80 -> "battery_80.bmp";
                100 -> "battery_100.bmp"
              end,
  case ets:member(btry_db,Battery_Pos) of
    true ->
      [{Battery_Pos,BatteryBMP_prev}] = ets:lookup(btry_db,Battery_Pos),
      wxBitmap:destroy(BatteryBMP_prev);
    false -> ok
  end,
  BatteryBMP = wxBitmap:new(State_img),
  ets:insert(btry_db,{Battery_Pos,BatteryBMP}),
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
  wxWindow:refresh(Panel,[{eraseBackground,false}]),{noreply, State},
  {noreply, State}.

handle_event(#wx{event = #wxMouse{type=left_down, x=X0, y=Y0}},State) -> % when the right click was pressed, activate a function that returns the color of the light that was pressed
  X = trunc(X0/20) * 20,
  Y = trunc(Y0/20) * 20,
  Battery_state = ets:lookup(btry_db,{X,Y}),
  case Battery_state of
    [] -> ok;
    [{_Battery_Pos,BatteryBMP}] ->
      ets:insert(screen_db,{battery,{{X-5,Y-20},BatteryBMP}}),
      timer:apply_after(1000,ets,delete,[screen_db, {X-5,Y-20}])
  end,
  {noreply,State};

handle_event(#wx{event = #wxClose{}},State = #main_PC_state {frame = Frame}) -> % close window event
  io:format("Exiting\n"),
  wxWindow:destroy(Frame),
  wx:destroy(),
  {stop,normal,State};

handle_event(_Event,State) -> % when left click has been pressed, activate navigation function
  {noreply,State}.


%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #main_PC_state{}) -> term()).
terminate(_Reason, _State = #main_PC_state{}) ->
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
drawETS(Painter) ->
  First = ets:first(screen_db),
  case First of
    '$end_of_table'-> ok;
    battery ->
      [{_Battery,Battery_State}] = ets:lookup(screen_db,First),
      drawETS(Painter,First,Battery_State);
    _ ->
      [{Sensor_Pos,SensorBMP}] = ets:lookup(screen_db,First),
      wxDC:drawBitmap(Painter, SensorBMP, Sensor_Pos),
      drawETS(Painter,First,none)
  end.
drawETS(Painter,Curr,Battery) ->
  Next = ets:next(screen_db,Curr),
  case Next of
    '$end_of_table' ->
      case Battery of
        none -> ok;
        {Battery_Pos,BatteryBMP} ->
          wxDC:drawBitmap(Painter, BatteryBMP, Battery_Pos)
      end;
    battery ->
      [{_Battery,Battery_State}] = ets:lookup(screen_db,Next),
      drawETS(Painter,Next,Battery_State);
    _ ->
      [{Sensor_Pos,SensorBMP}] = ets:lookup(screen_db,Next),
      wxDC:drawBitmap(Painter, SensorBMP, Sensor_Pos),
      drawETS(Painter,Next,Battery)
  end.
