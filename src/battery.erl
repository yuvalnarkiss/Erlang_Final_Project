%%%-------------------------------------------------------------------
%%% @author ubuntu
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jul 2021 7:40 PM
%%%-------------------------------------------------------------------
-module(battery).
-author("ubuntu").

-include("params.hrl").

-export([start_battery/1,start_battery/3]).

start_battery(Name) ->
	put(baterry_lvl,?FULL_BATTERY),
	batteryMode(100.0,sleep,Name).

start_battery(Name,State,Battery_level) ->
	put(baterry_lvl,Battery_level),
	batteryMode(Battery_level/1,State,Name).

batteryMode(Battery_lvl,_,Sensor_ID) when Battery_lvl =< 0 ->
	%Battery: Battery level 0% Power Off
	sensor:power_off(Sensor_ID);
batteryMode(Battery_lvl,awake,Sensor_ID) ->
	New_Battery_lvl = battery_activity(Sensor_ID,Battery_lvl,100,6),				%awake for 0.6 s before sendig data and going back to sleep
	Sending_stat = sensor:gotoSleep(Sensor_ID),
	Penalty = case Sending_stat of
							sent -> 1.0;
							not_sent -> 0.0
						end,
	batteryMode(New_Battery_lvl - Penalty,sleep,Sensor_ID);

batteryMode(Battery_lvl,sleep,Sensor_ID) ->
	New_Battery_lvl = battery_activity(Sensor_ID,Battery_lvl,600,1),				%sleep for 0.6 s before randomizing P
	%Battery: call sensor with request 'randomize_P'
	Next_state = sensor:randomize_P(Sensor_ID),
	case Next_state of
		sleep -> batteryMode(New_Battery_lvl,sleep,Sensor_ID);
		awake -> batteryMode(New_Battery_lvl - 0.4,awake,Sensor_ID)		% 0.4 percent penalty for monitoring data
	end.


battery_activity(_Sensor_ID,Battery_lvl,_Timeout,0) ->	Battery_lvl;
battery_activity(Sensor_ID,Battery_lvl,Timeout,Battery_drop) ->
	timer:sleep(Timeout),
	New_Battery_lvl = Battery_lvl - 0.2,
	Round_Battery_lvl = trunc(math:ceil(New_Battery_lvl)),
	case (get(baterry_lvl) - Round_Battery_lvl) >= 5 of
		true ->
			sensor:set_battery(Sensor_ID,Round_Battery_lvl),
			put(baterry_lvl,Round_Battery_lvl);
		false -> ok
	end,
	battery_activity(Sensor_ID, New_Battery_lvl, Timeout, Battery_drop - 1).





%% API