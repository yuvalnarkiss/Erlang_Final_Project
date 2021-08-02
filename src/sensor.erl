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
-export([start/1]).

%% gen_statem callbacks
-export([init/1, format_status/2, gotoSleep/1, randomize_P/1, power_off/1, update_neighbors/2, idle/3, sleep/3, awake/3, handle_event/4, terminate/3,
  code_change/4, callback_mode/0]).

-record('Sensor', {}).

-define(SERVER, ?MODULE).

%%-record('Sensor'_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start(Sensor_Pos) ->
	%{ok, Sensor_PID} =
	gen_statem:start(?MODULE, Sensor_Pos, []).
	%ets:insert(db,{Sensor_Pos, Sensor_PID}).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================
update_neighbors(Sensor_Name,NhbrList) ->
	gen_statem:cast(Sensor_Name,{update_neighbors,NhbrList}).

gotoSleep(Sensor_Name) ->
	gen_statem:call(Sensor_Name,gotoSleep).

randomize_P(Sensor_Name) ->
	gen_statem:call(Sensor_Name,randomize_P).

power_off(Sensor_Name) ->
	gen_statem:stop(Sensor_Name).
%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init(Sensor_Pos) ->
  Data = #{name => self(), position => Sensor_Pos, compared_P => 0.7, neighbors => [], data_list => []},
	io:format("Sensor ~p initiated~n", [self()]), %ToDo:Temp comment
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

idle(cast,{update_neighbors,NhbrList}, #{name := Name, position := Sensor_Pos} = Data) ->
	%start battery fsm with 100%
	spawn_link(battery,batteryMode,[100,sleep,Name]),
	main_PC:update_sensor({Sensor_Pos,asleep}),
	{next_state,sleep,Data#{neighbors := NhbrList}};
idle({call,From}, {forward,_Data_List}, _Data) ->
	{keep_state_and_data,[{reply,From,abort}]}.	%another sensor tried to send data to this sensor while in sleep mode - data not received

sleep({call,From}, randomize_P, #{position := Sensor_Pos, compared_P := P_comp, data_list := Data_List} = Data) ->
	P = rand:uniform(10)/10,  % uniformly randomized floating number between 0.0 - 1.0
	%io:format("Sensor: randomized p = ~p ~n", [P]), %ToDo:Temp comment
	{Next_State, New_Data_List} = case P > P_comp of
		true ->
			Data_map = maps:new(),
			main_PC:update_sensor({Sensor_Pos,active}),
			%%ToDo: monitor data and save in map,
			{awake, Data_List ++ [Data_map]};
		false ->
			{sleep, Data_List}
	end,
	%io:format("Sensor: next state = ~p, data list = ~p~n", [Next_State,New_Data_List]), %ToDo:Temp comment
	{next_state,Next_State,Data#{data_list := New_Data_List},[{reply,From,Next_State}]};
sleep({call,From}, {forward,_Data_List}, _Data) ->
	{keep_state_and_data,[{reply,From,abort}]}.	%another sensor tried to send data to this sensor while in sleep mode - data not received


awake({call,From}, gotoSleep, #{position := Sensor_Pos, neighbors := NhbrList, data_list := Data_List} = Data) ->
	%%ToDo: we need to think if we want to send all the neighbors the data or one neighbor is enough (to know whether to save the map or earase)
	io:format("Sensor ~p: sending data to neighbors ~p ~n", [self(),NhbrList]), %ToDo:Temp comment
	New_Data_List = send_data_to_neighbor(Sensor_Pos,NhbrList,Data_List),
	%io:format("Sensor: new data list ~p ~n", [New_Data_List]), %ToDo:Temp comment
	main_PC:update_sensor({Sensor_Pos,asleep}),
	{next_state,sleep,Data#{data_list := New_Data_List},[{reply,From,ok}]};
awake({call,From}, {forward,{From_SensorInPos,Rec_Data_List}}, #{data_list := Data_List} = Data) ->
	io:format("Sensor ~p: got data from ~p ~n", [self(),From]), %ToDo:Temp comment
	main_PC:update_sensor({From_SensorInPos,sending}),
	timer:sleep(1000),
	New_Data_List = Data_List ++ Rec_Data_List,
	main_PC:update_sensor({From_SensorInPos,active}),
	timer:sleep(600),
	io:format("Sensor ~p: updated data list =~p ~n", [self(),New_Data_List]), %ToDo:Temp comment
	{keep_state,Data#{data_list := New_Data_List},[{reply,From,sent}]}.



%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
handle_event(_EventType, _EventContent, _StateName, State = #'Sensor'{}) ->
NextStateName = the_next_state_name,
{next_state, NextStateName, State}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, #{position := Sensor_Pos}) ->
	io:format("Sensor ~p: shutting down ~n", [self()]), %ToDo:Temp comment
	main_PC:update_sensor({Sensor_Pos,inactive}),
ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #'Sensor'{}, _Extra) ->
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
		sent -> io:format("Sensor ~p: success! data sent to ~p ~n", [self(),Neighbor_PID]), [];	%ToDo:Temp comment
		abort -> io:format("Sensor ~p: failed to send data to ~p ~n", [self(),Neighbor_PID]), send_data_to_neighbor(Sensor_Pos,NhbrList,Data_List)	 %ToDo:Temp comment
	end,
	New_Data_List.