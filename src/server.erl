%%%-------------------------------------------------------------------
%%% @author katrinn
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Aug 2021 22:45
%%%-------------------------------------------------------------------
-module(server).
-author("katrinn").

-behaviour(gen_server).

%% API
-export([start_link/2,updateETS/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(server_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link( MainPC_ID :: pid() , Which_PC :: atom()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(MainPC_ID,Which_PC) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {MainPC_ID,Which_PC}, []).

updateETS(Sensor_Pos,Sensor_Data) ->    % Sensor_Data = {State,Neighbors,P_comp,Battery_level,Data_list}
  gen_server:cast(?SERVER, {update_ets,Sensor_Pos,Sensor_Data}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #server_state{}} | {ok, State :: #server_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init({MainPC_ID,Which_PC}) ->
  register(main_PC,MainPC_ID),
  Offset = case Which_PC of
             pc1 -> {0,0};
             pc2 -> {480,0};
             pc3 -> {0,420};
             pc4 -> {480,420}
           end,
  ets:new(data_base,[set,public,named_table,{heir, main_PC, heirData}]),
  Num_of_sensors = rand:uniform(523) + 5, % number of sensors randomized between 6 - 576
  Pos_list = randomize_positions(Num_of_sensors,Offset),
  Sensor_PID_Pos_list = create_sensors(Pos_list),
  main_PC ! {sens_list,Sensor_PID_Pos_list},
  {ok, #server_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #server_state{}) ->
  {reply, Reply :: term(), NewState :: #server_state{}} |
  {reply, Reply :: term(), NewState :: #server_state{}, timeout() | hibernate} |
  {noreply, NewState :: #server_state{}} |
  {noreply, NewState :: #server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #server_state{}} |
  {stop, Reason :: term(), NewState :: #server_state{}}).
handle_call(_Request, _From, State = #server_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #server_state{}) ->
  {noreply, NewState :: #server_state{}} |
  {noreply, NewState :: #server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #server_state{}}).
handle_cast({update_ets,Sensor_Pos,Sensor_Data}, State = #server_state{}) ->
  ets:insert(data_base, {Sensor_Pos,Sensor_Data}),
  {noreply, State};
handle_cast(_Request, State = #server_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #server_state{}) ->
  {noreply, NewState :: #server_state{}} |
  {noreply, NewState :: #server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #server_state{}}).
handle_info({'ETS-TRANSFER', TableId, _OldOwner, _GiftData}, State = #server_state{}) ->
  ETS_old_list = ets:tab2list(TableId),
  recreate_sensors(ETS_old_list),
  {noreply, State};
handle_info(_Info, State = #server_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #server_state{}) -> term()).
terminate(_Reason, _State = #server_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #server_state{},
    Extra :: term()) ->
  {ok, NewState :: #server_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #server_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

randomize_positions(Num_of_sensors,Offset) ->
  randomize_positions([],Num_of_sensors,Offset).

randomize_positions(Pos_List,0,_Offset) ->
  % get rid of duplicates
  Pos_Set = sets:from_list(Pos_List),
  sets:to_list(Pos_Set);
randomize_positions(Pos_List,Num_of_sensors,{OffsetX,OffsetY} = Offset) ->
  X = 20 * (rand:uniform(24) - 1) + OffsetX,
  Y = 20 * (rand:uniform(22) - 1) + OffsetY,
  randomize_positions([{X,Y} | Pos_List],Num_of_sensors-1,Offset).

create_sensors([]) -> [];
create_sensors([{X,Y}|Pos_list]) when ( X >= 920 ) and ( Y =< 60 ) -> create_sensors(Pos_list);   % Don't create sensor on the stationary_comp
create_sensors([Position|Pos_list]) ->
  {ok, Sensor_PID} = sensor:start(Position),
  [{Sensor_PID, Position} | create_sensors(Pos_list)].

recreate_sensors([]) -> ok;
recreate_sensors([{Sensor_Pos,Sensor_Data}|Sensors_list]) ->
  sensor:start(Sensor_Pos,Sensor_Data),
  ets:insert(data_base,{Sensor_Pos,Sensor_Data}),
  recreate_sensors(Sensors_list).
