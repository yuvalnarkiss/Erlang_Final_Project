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
-include("params.hrl").

-define(CALL_TIMEOUT, 200).
-define(SLEEP_PROBABILITY,98).
-define(MAX_NEIGHBORS,7).

-define(BLUE_LIGHT,900).
-define(TURN_OFF_BLUE_LIGHT,300).

-define(TEMP_RANGE,31).
-define(SELF_TEMP_RANGE,50).
-define(MIN_TEMP,9).


%% API
-export([start_link/2,start_link/3, gotoSleep/1, set_battery/2, randomize_P/1, forward/2, power_off/1, update_neighbors/2, sensor_down/1]).

%% gen_statem callbacks
-export([init/1, format_status/2, idle/3, sleep/3, awake/3, dead/3, handle_event/4, terminate/3,
	code_change/4, callback_mode/0]).

-record(sensor, {main,name,position,compared_P,neighbors,battery_level,data_list,msg_count}).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link({X,Y} = Sensor_Pos,Main_PC) ->
	Sensor_Name = list_to_atom("sensor_" ++ integer_to_list(X) ++ integer_to_list(Y)),
	gen_statem:start_link({local,Sensor_Name}, ?MODULE, {Sensor_Pos,Sensor_Name,Main_PC}, []).
start_link({X,Y} = Sensor_Pos,Sensor_Data,Main_PC) ->
	Sensor_Name = list_to_atom("sensor_" ++ integer_to_list(X) ++ integer_to_list(Y)),
	gen_statem:start_link({local,Sensor_Name},?MODULE, {recover,Sensor_Pos,Sensor_Name,Sensor_Data,Main_PC}, []).

update_neighbors(Sensor_Name,NhbrList) ->
	gen_statem:cast(Sensor_Name,{update_neighbors,NhbrList}).

gotoSleep(Sensor_Name) ->
	gen_statem:call(Sensor_Name,gotoSleep).

set_battery(Sensor_Name,New_level) ->
	gen_statem:cast(Sensor_Name,{set_battery,New_level}).

randomize_P(Sensor_Name) ->
	gen_statem:call(Sensor_Name,randomize_P).

forward(Sensor_Name,Data) ->
	% catch the exception in case the sensor we want to communicate with is not created yet or was terminated
	try gen_statem:call(Sensor_Name,{forward,Data},{clean_timeout,?CALL_TIMEOUT})
	catch
		exit:{timeout,_Call} -> abort;
		exit:{{nodedown,_Node},_Call} -> stop
	end.

power_off(Sensor_Name) ->
	gen_statem:cast(Sensor_Name,power_off).

sensor_down(Sensor_Name) ->
	gen_statem:stop(Sensor_Name).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================
%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init({recover,Sensor_Pos,Sensor_Name,Sensor_Data,Main_PC}) ->
	process_flag(trap_exit,true),		%trap node_down
	{State,Neighbors,P_comp,Battery_level,Data_list,MSG_CNT} = Sensor_Data,
	Data = #sensor{main = Main_PC, name = Sensor_Name, position = Sensor_Pos, compared_P = P_comp, neighbors = Neighbors, battery_level = Battery_level, data_list = Data_list, msg_count = MSG_CNT},
	spawn_link(battery,start_battery,[Sensor_Name,State,Battery_level]),
	Graphic_State = case State of
										sleep -> asleep;
										awake -> active;
										dead -> inactive;
										_ -> active
									end,
	ets:insert(graphic_sensor,{Sensor_Pos,Graphic_State}),
	update_batery_img(Sensor_Pos,?FULL_BATTERY,Battery_level),
	{ok, State, Data};
init({Sensor_Pos,Sensor_Name,Main_PC}) ->
	process_flag(trap_exit,true),		%trap node_down
	P_comp = rand:uniform(4) + (?SLEEP_PROBABILITY - 4),  % percentage of sleep time randomize between 95%-98%
	Data = #sensor{main = Main_PC, name = Sensor_Name, position = Sensor_Pos, compared_P = P_comp, neighbors = [], battery_level = ?FULL_BATTERY, data_list = [], msg_count = 0},
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
%--------------------------------------------------
%							IDLE STATE
%-------------------------------------------------
idle(cast,{update_neighbors,NhbrList}, #sensor{name = Name, position = Sensor_Pos} = Data) ->
	Sorted_NhbrList = neighbor_sort(Sensor_Pos,NhbrList),
	spawn_link(battery,start_battery,[Name]),

	ets:insert(graphic_sensor,{Sensor_Pos,asleep}),
	ets:insert(graphic_battery,{Sensor_Pos,?FULL_BATTERY}),

	{next_state,sleep,Data#sensor{neighbors = Sorted_NhbrList}};

% Ignore other messages
idle({call,From}, {forward,_Data_List}, _Data) ->
	{keep_state_and_data,[{reply,From,abort}]};	%another sensor tried to send data to this sensor while in idle mode - data not received
idle(info, {'EXIT',_PID,_Reason}, _Data) ->
	keep_state_and_data;
idle(info, _Info, _Data) ->
	keep_state_and_data.

%--------------------------------------------------
%							SLEEP STATE
%-------------------------------------------------
sleep({call,From}, randomize_P, #sensor{position = Sensor_Pos, compared_P = P_comp, data_list = Data_List, msg_count = MSG_Count, battery_level = Battery_level} = Data) ->
	P = rand:uniform(100),  % uniformly randomized floating number between 1 - 100
	{Next_State, New_Data} = case P > P_comp of
														 true ->
															 ets:insert(graphic_sensor,{Sensor_Pos,active}),
															 Data_map = maps:merge(monitor_data(Sensor_Pos), #{message_count => MSG_Count, battery => Battery_level}),
															 ets:insert(data_base, {Sensor_Pos,{awake,Data#sensor.neighbors,P_comp,Data#sensor.battery_level,Data_List ++ [Data_map],MSG_Count}}),
															 {awake, Data#sensor{data_list = Data_List ++ [Data_map]}};
														 false ->
															 {sleep, Data}
													 end,
	{next_state,Next_State,New_Data,[{reply,From,Next_State}]};
sleep(cast, {set_battery,New_level}, #sensor{position = Sensor_Pos, battery_level = Old_Battery_Level, msg_count = MSG_Count} = Data) ->
	ets:insert(data_base, {Data#sensor.position,{sleep,Data#sensor.neighbors,Data#sensor.compared_P,New_level,Data#sensor.data_list,MSG_Count}}),
	update_batery_img(Sensor_Pos,Old_Battery_Level,New_level),
	{keep_state,Data#sensor{battery_level = New_level}};
sleep(cast, power_off, #sensor{position = Sensor_Pos} = Data) ->
	ets:insert(graphic_sensor,{Sensor_Pos,inactive}),
	ets:insert(data_base, {Sensor_Pos,{dead,[],Data#sensor.compared_P,0,[],0}}),
	{next_state,dead,Data#sensor{ neighbors = [], data_list = [], battery_level =0}};

% Ignore other messages
sleep({call,From}, {forward,_Data_List}, _Data) ->
	{keep_state_and_data,[{reply,From,abort}]};	%another sensor tried to send data to this sensor while in sleep mode - data not received
sleep(info, {'EXIT',_PID,_Reason}, _Data) ->
	keep_state_and_data;
sleep(info, _Info, _Data) ->
	keep_state_and_data.

%--------------------------------------------------
%							AWAKE STATE
%-------------------------------------------------
awake({call,From}, gotoSleep, #sensor{main = Main_PC, position = Sensor_Pos, neighbors = NhbrList, data_list = Data_List, msg_count = MSG_Count} = Data) ->
	{New_Data_List, Msg_status} = send_data_to_neighbor(Sensor_Pos,NhbrList,Data_List,Main_PC),
	{Reply, NEW_MSG_Count} = case Msg_status of
						sent -> {sent, MSG_Count + 1};
						_ -> {not_sent, MSG_Count}
					end,
	ets:insert(data_base, {Sensor_Pos,{sleep,NhbrList,Data#sensor.compared_P,Data#sensor.battery_level,New_Data_List,NEW_MSG_Count}}),
	ets:insert(graphic_sensor,{Sensor_Pos,asleep}),
	{next_state,sleep,Data#sensor{data_list = New_Data_List, msg_count = NEW_MSG_Count},[{reply,From,Reply}]};
awake({call,From}, {forward,{From_SensorInPos,Rec_Data_List}}, #sensor{position = Pos, data_list = Data_List} = Data) ->
	ets:insert(graphic_sensor,{Pos,sending}),
	ets:insert(graphic_sensor,{From_SensorInPos,sending}),
	timer:sleep(?BLUE_LIGHT),	%for graphic purposes
	New_Data_List = Data_List ++ Rec_Data_List,
	ets:insert(graphic_sensor,{Pos,active}),
	ets:delete(graphic_sensor,From_SensorInPos),
	timer:sleep(?TURN_OFF_BLUE_LIGHT),		%for graphic purposes
	{keep_state,Data#sensor{data_list = New_Data_List},[{reply,From,sent}]};
awake(cast, {set_battery,New_level}, #sensor{position = Sensor_Pos, battery_level = Old_Battery_Level} = Data) ->
	ets:insert(data_base, {Data#sensor.position,{awake,Data#sensor.neighbors,Data#sensor.compared_P,New_level,Data#sensor.data_list,Data#sensor.msg_count}}),
	update_batery_img(Sensor_Pos,Old_Battery_Level,New_level),
	{keep_state,Data#sensor{battery_level = New_level}};
awake(cast, power_off, #sensor{position = Sensor_Pos} = Data) ->
	ets:insert(graphic_sensor,{Sensor_Pos,inactive}),
	ets:insert(data_base, {Sensor_Pos,{dead,[],Data#sensor.compared_P,0,[],0}}),
	{next_state,dead,Data#sensor{ neighbors = [], data_list = [], battery_level = 0}};

% ignore other messages
awake(info, {'EXIT',_PID,_Reason}, _Data) ->
	keep_state_and_data;
awake(info, _Info, _Data) ->
	keep_state_and_data.

%--------------------------------------------------
%							DEAD STATE
%-------------------------------------------------
dead({call,From}, {forward,_Data_List}, _Data) ->
	{keep_state_and_data,[{reply,From,abort}]};	%another sensor tried to send data to this sensor while in sleep mode - data not received
dead(info, {'EXIT',_PID,_Reason}, _Data) ->
	keep_state_and_data;	%catch exit
dead(info, _Info, _Data) ->
	keep_state_and_data.


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
	io:format("Sensor ~p: shutting down ~n", [Sensor_Name]),
	ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #sensor{}, _Extra) ->
	{ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
send_data_to_neighbor(_Sensor_Pos,[],Data_List,_Tuple) -> {Data_List, not_sent};
send_data_to_neighbor(_Sensor_Pos,[{_Stationary_comp, ?STATIONARY_COMP_POS}|_NhbrList],Data_List,Main_PC) ->
	{stationary_comp,Main_PC} ! {data,Data_List}, {[], sent};
send_data_to_neighbor(Sensor_Pos,[{Neighbor_PID,Neighbor_POS}|NhbrList],Data_List,Main_PC) ->
	Node = find_pc(Neighbor_POS),
	Self_Node = find_pc(Sensor_Pos),
	Msg_status = case Node == Self_Node of
		true ->  sensor:forward(Neighbor_PID,{Sensor_Pos,Data_List});
		false -> rpc:call(Node,sensor,forward,[Neighbor_PID,{Sensor_Pos,Data_List}])
	end,
	{New_Data_List, Final_status} = case Msg_status of
										sent -> {[], sent};
										abort -> send_data_to_neighbor(Sensor_Pos,NhbrList,Data_List,Main_PC);
										{badrpc,{'EXIT',{timeout,_Data}}} -> send_data_to_neighbor(Sensor_Pos,NhbrList,Data_List,Main_PC);
										{badrpc,_Reason} -> {Data_List, not_sent};
										stop -> {Data_List, stop}
									end,
	{New_Data_List, Final_status}.

monitor_data(Sensor_Pos) ->
	Time = erlang:universaltime(),
	Temp = rand:uniform(?TEMP_RANGE) + ?MIN_TEMP,
	Self_Temp = Temp + rand:uniform(?SELF_TEMP_RANGE),
	Humidity = rand:uniform(100),
	#{position => Sensor_Pos, time => Time , temp => Temp, self_temp => Self_Temp, humidity => Humidity}.

neighbor_sort(_Sensor_Pos,[]) -> [];
neighbor_sort(Sensor_Pos,NhbrList) ->
	Target = ?STATIONARY_COMP_POS,
	Dist_list = [ {dist(Sensor_Pos,Mid_sens_pos) + dist(Target,Mid_sens_pos), Mid_sens_pid,Mid_sens_pos} || {Mid_sens_pid,Mid_sens_pos} <- NhbrList ],
	Sorted_Dist_list = lists:sort(fun({A,_Pid1,_Pos1},{B,_Pid2,_Pos2}) -> A =< B end, Dist_list),
	Sorted_NhbrList = [ {Sens_pid,Sens_pos} || {_Dist, Sens_pid,Sens_pos} <- Sorted_Dist_list ],
	case length(Sorted_NhbrList) > ?MAX_NEIGHBORS of
		true -> {List1,_List2} = lists:split(?MAX_NEIGHBORS,Sorted_NhbrList), List1;
		false -> Sorted_NhbrList
	end.

dist({X1,Y1},{X2,Y2}) -> trunc(math:ceil(math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)))).

update_batery_img(Sensor_Pos,Old_Battery_Level0,New_Battery_Level0) ->
	Old_Battery_Level = trunc(math:ceil(Old_Battery_Level0/20) * 20),
	New_Battery_Level = trunc(math:ceil(New_Battery_Level0/20) * 20),
	case Old_Battery_Level /= New_Battery_Level of
		true ->
			ets:insert(graphic_battery,{Sensor_Pos,New_Battery_Level});
		false ->
			case  (New_Battery_Level0 =< 15) and (Old_Battery_Level0 > 15) of
				true ->
					ets:insert(graphic_battery,{Sensor_Pos,low_battery});
				false -> ok
			end
	end.

find_pc(Sensor_Pos) ->
	case Sensor_Pos of
		{X,Y} when X < ?ULQXBOUNDARY , Y < ?ULQYBOUNDARY -> ets:lookup_element(nodes,pc1,2);
		{X,Y} when X > ?URQXBOUNDARY , Y < ?URQYBOUNDARY -> ets:lookup_element(nodes,pc2,2);
		{X,Y} when X < ?DLQXBOUNDARY , Y > ?DLQYBOUNDARY -> ets:lookup_element(nodes,pc3,2);
		{X,Y} when X > ?DRQXBOUNDARY , Y > ?DRQYBOUNDARY -> ets:lookup_element(nodes,pc4,2)
	end.
