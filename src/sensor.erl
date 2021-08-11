%%%-------------------------------------------------------------------
%%% @author ubuntu
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jul 2021 5:08 PM
%%%-------------------------------------------------------------------
-module(sensor).
-author("ubuntu").

-behaviour(gen_statem).

%% API
-export([start/2,start/3]).

%% gen_statem callbacks
-export([init/1, format_status/2, gotoSleep/1, set_battery/2, randomize_P/1, forward/2, power_off/1, update_neighbors/2, idle/3, sleep/3, awake/3, dead/3, handle_event/4, terminate/3,
	code_change/4, callback_mode/0]).

-record(sensor, {main,nodes,name,position,compared_P,neighbors,battery_level,data_list}).

-define(SERVER, ?MODULE).

-define(ULQXBOUNDARY, 480).
-define(ULQYBOUNDARY, 440).
-define(URQXBOUNDARY, 460).
-define(URQYBOUNDARY, 440).
-define(DLQXBOUNDARY, 480).
-define(DLQYBOUNDARY, 420).
-define(DRQXBOUNDARY, 460).
-define(DRQYBOUNDARY, 420).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start({X,Y} = Sensor_Pos,{Main_PC,PC_list}) ->
	Sensor_Name = list_to_atom("sensor_" ++ integer_to_list(X) ++ integer_to_list(Y)),
	gen_statem:start({local,Sensor_Name}, ?MODULE, {Sensor_Pos,Sensor_Name,{Main_PC,PC_list}}, []).
start({X,Y} = Sensor_Pos,Sensor_Data,{Main_PC,PC_list}) ->
	Sensor_Name = list_to_atom("sensor_" ++ integer_to_list(X) ++ integer_to_list(Y)),
	gen_statem:start({local,Sensor_Name},?MODULE, {recover,Sensor_Pos,Sensor_Name,Sensor_Data,{Main_PC,PC_list}}, []).

update_neighbors(Sensor_Name,NhbrList) ->
	gen_statem:cast(Sensor_Name,{update_neighbors,NhbrList}).

gotoSleep(Sensor_Name) ->
	gen_statem:call(Sensor_Name,gotoSleep).

set_battery(Sensor_Name,New_level) ->
	gen_statem:cast(Sensor_Name,{set_battery,New_level}).

randomize_P(Sensor_Name) ->
	gen_statem:call(Sensor_Name,randomize_P).

forward(Sensor_Name,Data) ->
	try gen_statem:call(Sensor_Name,{forward,Data},{clean_timeout,100})
	catch
		exit:{timeout,_Call} -> abort
	end.

power_off(Sensor_Name) ->
	gen_statem:cast(Sensor_Name,power_off).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================
%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init({recover,Sensor_Pos,Sensor_Name,Sensor_Data,{Main_PC,PC_List}}) ->
	{State,Neighbors,P_comp,Battery_level,Data_list} = Sensor_Data,
	Data = #sensor{main = Main_PC, nodes = PC_List, name = Sensor_Name, position = Sensor_Pos, compared_P = P_comp, neighbors = Neighbors, battery_level = Battery_level, data_list = Data_list},
	spawn_link(battery,start_battery,[Sensor_Name,State,Battery_level]),
	Graphic_State = case State of
										sleep -> asleep;
										awake -> active
									end,
	rpc:call(Main_PC,graphic,update_sensor,[{Sensor_Pos,Graphic_State}]),
	update_batery_img(Main_PC,Sensor_Pos,100,Battery_level),
	{ok, State, Data};
init({Sensor_Pos,Sensor_Name,{Main_PC,PC_List}}) ->
	P_comp = rand:uniform(4) + 94,  % percentage of sleep time randomize between 95%-98%
	Data = #sensor{main = Main_PC, nodes = PC_List, name = Sensor_Name, position = Sensor_Pos, compared_P = P_comp, neighbors = [], battery_level = 100, data_list = []},
	{ok, idle, Data}.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
	state_functions.

%% @private
%% @doc Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
format_status(_Opt, [_PDict, _StateName, _State]) ->
	Status = some_term,
	Status.

%% @private
%% @doc There should be one instance of this function for each possible
%% state name.  If callback_mode is state_functions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.

idle(cast,{update_neighbors,NhbrList}, #sensor{main = Main_PC, name = Name, position = Sensor_Pos} = Data) ->
	Sorted_NhbrList = neighbor_sort(Sensor_Pos,NhbrList),
	spawn_link(battery,start_battery,[Name]),
	rpc:call(Main_PC,graphic,update_sensor,[{Sensor_Pos,asleep}]),
	rpc:call(Main_PC,graphic,update_battery,[{Sensor_Pos,100}]),
	{next_state,sleep,Data#sensor{neighbors = Sorted_NhbrList}};
idle({call,From}, {forward,_Data_List}, _Data) ->
	{keep_state_and_data,[{reply,From,abort}]}.	%another sensor tried to send data to this sensor while in sleep mode - data not received

sleep({call,From}, randomize_P, #sensor{main = Main_PC, position = Sensor_Pos, compared_P = P_comp, data_list = Data_List} = Data) ->
	P = rand:uniform(100),  % uniformly randomized floating number between 1 - 100
	{Next_State, New_Data} = case P > P_comp of
														 true ->
															 rpc:call(Main_PC,graphic,update_sensor,[{Sensor_Pos,active}]),
															 Data_map = monitor_data(Sensor_Pos),
															 ets:insert(data_base, {Sensor_Pos,{awake,Data#sensor.neighbors,P_comp,Data#sensor.battery_level,Data_List ++ [Data_map]}}),
															 {awake, Data#sensor{data_list = Data_List ++ [Data_map]}};
														 false ->
															 {sleep, Data}
													 end,
	{next_state,Next_State,New_Data,[{reply,From,Next_State}]};
sleep({call,From}, {forward,_Data_List}, _Data) ->
	{keep_state_and_data,[{reply,From,abort}]};	%another sensor tried to send data to this sensor while in sleep mode - data not received
sleep(cast, {set_battery,New_level}, #sensor{main = Main_PC, position = Sensor_Pos, battery_level = Old_Battery_Level} = Data) ->
	ets:insert(data_base, {Data#sensor.position,{sleep,Data#sensor.neighbors,Data#sensor.compared_P,New_level,Data#sensor.data_list}}),
	update_batery_img(Main_PC,Sensor_Pos,Old_Battery_Level,New_level),
	{keep_state,Data#sensor{battery_level = New_level}};
sleep(cast, power_off, #sensor{main = Main_PC, position = Sensor_Pos} = Data) ->
	rpc:call(Main_PC,graphic,update_sensor,[{Sensor_Pos,inactive}]),
	{next_state,dead,Data}.


awake({call,From}, gotoSleep, #sensor{main = Main_PC, nodes = PC_List, position = Sensor_Pos, neighbors = NhbrList, data_list = Data_List} = Data) ->
	New_Data_List = send_data_to_neighbor(Sensor_Pos,NhbrList,Data_List,{Main_PC,PC_List}),
	Reply = case New_Data_List of
						[] -> sent;
						_ -> not_sent
					end,
	ets:insert(data_base, {Sensor_Pos,{sleep,NhbrList,Data#sensor.compared_P,Data#sensor.battery_level,New_Data_List}}),
	rpc:call(Main_PC,graphic,update_sensor,[{Sensor_Pos,asleep}]),
	{next_state,sleep,Data#sensor{data_list = New_Data_List},[{reply,From,Reply}]};
awake({call,From}, {forward,{From_SensorInPos,Rec_Data_List}}, #sensor{main = Main_PC, data_list = Data_List} = Data) ->
	rpc:call(Main_PC,graphic,update_sensor,[{From_SensorInPos,sending}]),
	timer:sleep(900),	%for graphic purposes
	New_Data_List = Data_List ++ Rec_Data_List,
	rpc:call(Main_PC,graphic,update_sensor,[{From_SensorInPos,active}]),
	timer:sleep(300),		%for graphic purposes
	{keep_state,Data#sensor{data_list = New_Data_List},[{reply,From,sent}]};
awake(cast, {set_battery,New_level}, #sensor{main = Main_PC, position = Sensor_Pos, battery_level = Old_Battery_Level} = Data) ->
	ets:insert(data_base, {Data#sensor.position,{awake,Data#sensor.neighbors,Data#sensor.compared_P,New_level,Data#sensor.data_list}}),
	update_batery_img(Main_PC,Sensor_Pos,Old_Battery_Level,New_level),
	{keep_state,Data#sensor{battery_level = New_level}};
awake(cast, power_off, #sensor{main = Main_PC, position = Sensor_Pos} = Data) ->
	rpc:call(Main_PC,graphic,update_sensor,[{Sensor_Pos,inactive}]),
	{next_state,dead,Data}.

dead({call,From}, {forward,_Data_List}, _Data) ->
	{keep_state_and_data,[{reply,From,abort}]}.	%another sensor tried to send data to this sensor while in sleep mode - data not received


%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
handle_event(_EventType, _EventContent, _StateName, State = #sensor{}) ->
	NextStateName = the_next_state_name,
	{next_state, NextStateName, State}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, #sensor{name = Sensor_Name}) ->
	io:format("Sensor ~p: shutting down ~n", [Sensor_Name]), %ToDo:Temp comment
	ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #sensor{}, _Extra) ->
	{ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
send_data_to_neighbor(_Sensor_Pos,[],Data_List,_Tuple) -> Data_List;
send_data_to_neighbor(_Sensor_Pos,[{_Stationary_comp, {940,0}}|_NhbrList],Data_List,{Main_PC,_PC_list}) ->
	{stationary_comp,Main_PC} ! {data,Data_List},[];
send_data_to_neighbor(Sensor_Pos,[{Neighbor_PID,Neighbor_POS}|NhbrList],Data_List,{Main_PC,PC_list}) ->
	Node = find_pc(Neighbor_POS,PC_list),
	Self_Node = find_pc(Sensor_Pos,PC_list),
	Msg_status = case Node == Self_Node of
		true ->  sensor:forward(Neighbor_PID,{Sensor_Pos,Data_List});
		false -> rpc:call(Node,server,forward,[{Neighbor_PID,Sensor_Pos,Data_List}])
	end,
	New_Data_List = case Msg_status of
										sent -> [];
										abort -> send_data_to_neighbor(Sensor_Pos,NhbrList,Data_List,{Main_PC,PC_list});
										{badrpc,{'EXIT',{timeout,_Data}}} -> send_data_to_neighbor(Sensor_Pos,NhbrList,Data_List,{Main_PC,PC_list})
									end,
	New_Data_List.

monitor_data(Sensor_Pos) ->
	Time = erlang:universaltime(),
	Temp = rand:uniform(31) + 9,
	Self_Temp = Temp + rand:uniform(50),
	Humidity = rand:uniform(100),
	#{position => Sensor_Pos, time => Time , temp => Temp, self_temp => Self_Temp, humidity => Humidity}.

neighbor_sort(_Sensor_Pos,[]) -> [];
neighbor_sort(Sensor_Pos,NhbrList) ->
	Target = {940,0},
	Dist_list = [ {dist(Sensor_Pos,Mid_sens_pos) + dist(Target,Mid_sens_pos), Mid_sens_pid,Mid_sens_pos} || {Mid_sens_pid,Mid_sens_pos} <- NhbrList ],
	Sorted_Dist_list = lists:sort(fun({A,_Pid1,_Pos1},{B,_Pid2,_Pos2}) -> A =< B end, Dist_list),
	Sorted_NhbrList = [ {Sens_pid,Sens_pos} || {_Dist, Sens_pid,Sens_pos} <- Sorted_Dist_list ],
	case length(Sorted_NhbrList) > 5 of
		true -> {List1,_List2} = lists:split(5,Sorted_NhbrList), List1;
		false -> Sorted_NhbrList
	end.

dist({X1,Y1},{X2,Y2}) -> trunc(math:ceil(math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)))).

update_batery_img(Main_PC, Sensor_Pos,Old_Battery_Level0,New_Battery_Level0) ->
	Old_Battery_Level = trunc(Old_Battery_Level0/20) * 20,
	New_Battery_Level = trunc(New_Battery_Level0/20) * 20,
	case Old_Battery_Level /= New_Battery_Level of
		true ->
			rpc:call(Main_PC,graphic,update_battery,[{Sensor_Pos,New_Battery_Level}]);
		false ->
			case (Old_Battery_Level > 15) and (New_Battery_Level =< 15) of
				true ->
					rpc:call(Main_PC,graphic,update_battery,[{Sensor_Pos,low_battery}]);
				false -> ok
			end
	end.

find_pc(Sensor_Pos,[PC1,PC2,PC3,PC4]) ->
	case Sensor_Pos of
		{X,Y} when X < ?ULQXBOUNDARY , Y < ?ULQYBOUNDARY -> PC1;
		{X,Y} when X > ?URQXBOUNDARY , Y < ?URQYBOUNDARY -> PC2;
		{X,Y} when X < ?DLQXBOUNDARY , Y > ?DLQYBOUNDARY -> PC3;
		{X,Y} when X > ?DRQXBOUNDARY , Y > ?DRQYBOUNDARY -> PC4
	end.
