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
-export([start_link/3,updateETS/2, mergeETS/1, forward/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(server_state, {main_pc_node,pc_list}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link( MainPC_Node ::node() , PC_list :: list() , Which_PC :: atom()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(MainPC_Node,PC_list,Which_PC) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {MainPC_Node,PC_list,Which_PC}, []).

updateETS(Sensor_Pos,Sensor_Data) ->    % Sensor_Data = {State,Neighbors,P_comp,Battery_level,Data_list}
  gen_server:cast(?SERVER, {update_ets,Sensor_Pos,Sensor_Data}).

mergeETS(ETS_old_list) ->
  gen_server:cast(?SERVER, {merge_ets, ETS_old_list}).

forward(Data) ->
  gen_server:call(?SERVER, {forward, Data}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #server_state{}} | {ok, State :: #server_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init({MainPC_Node,PC_list,Which_PC}) ->
  io:format("server init ~n",[]),
  Offset = case Which_PC of
             pc1 -> {0,0};
             pc2 -> {480,0};
             pc3 -> {0,420};
             pc4 -> {480,420}
           end,
  ets:new(data_base,[set,public,named_table]),
  Num_of_sensors = rand:uniform(508) + 20, % number of sensors randomized between 20 - 576
  Pos_list = randomize_positions(Num_of_sensors,Offset),
  Sensor_PID_Pos_list = create_sensors(MainPC_Node,PC_list,Pos_list),
  {'main_PC',MainPC_Node} ! {sens_list,Sensor_PID_Pos_list},

  timer:send_interval(100, self(), update_main_ets),
  {ok, #server_state{main_pc_node = MainPC_Node, pc_list = PC_list}}.

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
handle_call({forward, {Dest_PID,Src_Pos,Data_List}}, _From, State) ->
  Stat = sensor:forward(Dest_PID,{Src_Pos,Data_List}),
  {reply, Stat, State};
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
handle_cast({merge_ets, ETS_old_list}, #server_state{main_pc_node = MainPC_Node, pc_list = PC_list} = State) ->
  recreate_sensors(MainPC_Node,PC_list,ETS_old_list),
  {noreply, State};
handle_cast(_Request, State = #server_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #server_state{}) ->
  {noreply, NewState :: #server_state{}} |
  {noreply, NewState :: #server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #server_state{}}).
handle_info(update_main_ets, #server_state{main_pc_node = MainPC_Node} = State) ->
  rpc:call(MainPC_Node,main_PC,periodic_sensor_status_update,[ets:tab2list(data_base),node()]),
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
  Sensors = registered(),
  [sensor:sensor_down(Sensor_Name) || Sensor_Name <- Sensors],
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

create_sensors(_MainPC_Node,_PC_list,[]) -> [];
create_sensors(MainPC_Node,PC_list,[{X,Y}|Pos_list]) when ( X >= 920 ) and ( Y =< 60 ) -> create_sensors(MainPC_Node,PC_list,Pos_list);   % Don't create sensor on the stationary_comp
create_sensors(MainPC_Node,PC_list,[Position|Pos_list]) ->
  {ok, Sensor_PID} = sensor:start_link(Position,{MainPC_Node,PC_list}),
  [{Sensor_PID, Position} | create_sensors(MainPC_Node,PC_list,Pos_list)].

recreate_sensors(_MainPC_Node,_PC_list,[]) -> ok;
recreate_sensors(MainPC_Node,PC_list,[{Sensor_Pos,Sensor_Data}|Sensors_list]) ->
  sensor:start_link(Sensor_Pos,Sensor_Data,{MainPC_Node,PC_list}),
  ets:insert(data_base,{Sensor_Pos,Sensor_Data}),
  recreate_sensors(MainPC_Node,PC_list,Sensors_list).

