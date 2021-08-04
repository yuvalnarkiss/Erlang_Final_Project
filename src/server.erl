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
-export([start_link/0,updateETS/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(server_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

updateETS(Sensor_Pos,Sensor_Data) ->    % Sensor_Data = {Name,Position,State,Neighbors,Battery_level,Data_list}
  gen_server:cast(?SERVER, {update_ets,Sensor_Pos,Sensor_Data}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #server_state{}} | {ok, State :: #server_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  ets:new(data_base,[set,public,named_table]),
  Num_of_sensors = random:uniform(571) + 5, % number of sensors randomized between 6 - 576
  Pos_list = randomize_positions(Num_of_sensors,0,0), %ToDo: offsets are temp
  Sensor_PID_Pos_list = create_sensors(Pos_list),
  %ToDo: call a function that sends <Sensor_PID_list> to the main_PC and waits for full tree.
  % The next psudo function represents what needs to be implemented in main_pc
  % G = new(),
  % add vertices to G, each labeled after sensor_pid + pos (and one comp_pid)
  % for each couple of vertices -> if dist(P1,P2) < Radius, add edge between them
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

randomize_positions(Num_of_sensors,OffsetX,OffsetY) ->
  randomize_positions([],Num_of_sensors,OffsetX,OffsetY).

randomize_positions(Pos_List,0,_OffsetX,_OffsetY) ->
  % get rid of duplicates
  Pos_Set = sets:from_list(Pos_List),
  sets:to_list(Pos_Set);
randomize_positions(Pos_List,Num_of_sensors,OffsetX,OffsetY) ->
  X = 20 * (random:uniform(24) - 1) + OffsetX,
  Y = 20 * (random:uniform(24) - 1) + OffsetX,
  randomize_positions([{X,Y} | Pos_List],Num_of_sensors-1,OffsetX,OffsetY).

create_sensors([]) -> [];
  create_sensors([Position|Pos_list]) ->
  {ok, Sensor_PID} = sensor:start(Position),
  [{Sensor_PID, Position} | create_sensors(Pos_list)].

