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
-export([start/1,start/2]).

%% gen_statem callbacks
-export([init/1, format_status/2, gotoSleep/1, set_battery/2, randomize_P/1, power_off/1, update_neighbors/2, idle/3, sleep/3, awake/3, handle_event/4, terminate/3,
  code_change/4, callback_mode/0]).

-record(sensor, {name,position,compared_P,neighbors,battery_level,data_list}).

-define(SERVER, ?MODULE).

%%-record(sensor_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start(Sensor_Pos) ->
	gen_statem:start(?MODULE, Sensor_Pos, []).
start(Sensor_Pos,Sensor_Data) ->
	gen_statem:start(?MODULE, {recover,Sensor_Pos,Sensor_Data}, []).

update_neighbors(Sensor_Name,NhbrList) ->
	gen_statem:cast(Sensor_Name,{update_neighbors,NhbrList}).

gotoSleep(Sensor_Name) ->
	gen_statem:call(Sensor_Name,gotoSleep).

set_battery(Sensor_Name,New_level) ->
	gen_statem:cast(Sensor_Name,{set_battery,New_level}).

randomize_P(Sensor_Name) ->
	gen_statem:call(Sensor_Name,randomize_P).

power_off(Sensor_Name) ->
	gen_statem:stop(Sensor_Name).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================
%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init({recover,Sensor_Pos,Sensor_Data}) ->
	{State,Neighbors,P_comp,Battery_level,Data_list} = Sensor_Data,
	Data = #sensor{name = self(), position = Sensor_Pos, compared_P = P_comp, neighbors = Neighbors, battery_level = Battery_level, data_list = Data_list},
	spawn_link(battery,start_battery,[self(),State,Battery_level]),
	Graphic_State = case State of
										sleep -> asleep;
										awake -> active
									end,
	graphic:update_sensor({Sensor_Pos,Graphic_State}),
	{ok, State, Data};
init(Sensor_Pos) ->
	P_comp = rand:uniform(4) + 94,  % percentage of sleep time randomize between 95%-98%
  Data = #sensor{name = self(), position = Sensor_Pos, compared_P = P_comp, neighbors = [], battery_level = 100, data_list = []},
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

idle(cast,{update_neighbors,NhbrList}, #sensor{name = Name, position = Sensor_Pos} = Data) ->
	Sorted_NhbrList = neighbor_sort(Sensor_Pos,NhbrList),
	spawn_link(battery,start_battery,[Name]),
	graphic:update_sensor({Sensor_Pos,asleep}),
	{next_state,sleep,Data#sensor{neighbors = Sorted_NhbrList}};
idle({call,From}, {forward,_Data_List}, _Data) ->
	{keep_state_and_data,[{reply,From,abort}]}.	%another sensor tried to send data to this sensor while in sleep mode - data not received

sleep({call,From}, randomize_P, #sensor{position = Sensor_Pos, compared_P = P_comp, data_list = Data_List} = Data) ->
	P = rand:uniform(100),  % uniformly randomized floating number between 1 - 100
	{Next_State, New_Data} = case P > P_comp of
		true ->
			graphic:update_sensor({Sensor_Pos,active}),
			Data_map = monitor_data(Sensor_Pos),
			server:updateETS(Sensor_Pos,{awake,Data#sensor.neighbors,P_comp,Data#sensor.battery_level,Data_List ++ [Data_map]}),
			{awake, Data#sensor{data_list = Data_List ++ [Data_map]}};
		false ->
			{sleep, Data}
	end,
	{next_state,Next_State,New_Data,[{reply,From,Next_State}]};
sleep({call,From}, {forward,_Data_List}, _Data) ->
	{keep_state_and_data,[{reply,From,abort}]};	%another sensor tried to send data to this sensor while in sleep mode - data not received
sleep(cast, {set_battery,New_level}, Data) ->
	server:updateETS(Data#sensor.position,{sleep,Data#sensor.neighbors,Data#sensor.compared_P,New_level,Data#sensor.data_list}),
	{keep_state,Data#sensor{battery_level = New_level}}.


awake({call,From}, gotoSleep, #sensor{position = Sensor_Pos, neighbors = NhbrList, data_list = Data_List} = Data) ->
	New_Data_List = send_data_to_neighbor(Sensor_Pos,NhbrList,Data_List),
	Reply = case New_Data_List of
						[] -> sent;
						_ -> not_sent
					end,
	server:updateETS(Sensor_Pos,{sleep,NhbrList,Data#sensor.compared_P,Data#sensor.battery_level,New_Data_List}),
	graphic:update_sensor({Sensor_Pos,asleep}),
	{next_state,sleep,Data#sensor{data_list = New_Data_List},[{reply,From,Reply}]};
awake({call,From}, {forward,{From_SensorInPos,Rec_Data_List}}, #sensor{data_list = Data_List} = Data) ->
	graphic:update_sensor({From_SensorInPos,sending}),
	timer:sleep(900),	%for graphic purposes
	New_Data_List = Data_List ++ Rec_Data_List,
	graphic:update_sensor({From_SensorInPos,active}),
	timer:sleep(300),		%for graphic purposes
	{keep_state,Data#sensor{data_list = New_Data_List},[{reply,From,sent}]};
awake(cast, {set_battery,New_level}, Data) ->
	server:updateETS(Data#sensor.position,{awake,Data#sensor.neighbors,Data#sensor.compared_P,New_level,Data#sensor.data_list}),
	{keep_state,Data#sensor{battery_level = New_level}}.



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
terminate(_Reason, _StateName, #sensor{position = Sensor_Pos}) ->
	io:format("Sensor ~p: shutting down ~n", [self()]), %ToDo:Temp comment
	graphic:update_sensor({Sensor_Pos,inactive}),
ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #sensor{}, _Extra) ->
{ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
send_data_to_neighbor(_Sensor_Pos,[],Data_List) -> Data_List;
send_data_to_neighbor(Sensor_Pos,[Neighbor_PID|NhbrList],Data_List) ->
	Msg_status = case is_process_alive(Neighbor_PID) of
								 true -> gen_statem:call(Neighbor_PID,{forward,{Sensor_Pos,Data_List}});
								 false -> abort
							 end,
	New_Data_List = case Msg_status of
		sent -> [];
		abort -> send_data_to_neighbor(Sensor_Pos,NhbrList,Data_List)
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
	Dist_list = [ {dist(Sensor_Pos,Mid_sens_pos) + dist(Target,Mid_sens_pos), Mid_sens_pid} || {Mid_sens_pid,Mid_sens_pos} <- NhbrList ],
	Sorted_Dist_list = lists:sort(fun({A,_Pid1},{B,_Pid2}) -> A =< B end, Dist_list),
	Serted_NhbrList = [ Sens_pid || {_Dist, Sens_pid} <- Sorted_Dist_list ],
	Serted_NhbrList.

dist({X1,Y1},{X2,Y2}) -> trunc(math:ceil(math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)))).