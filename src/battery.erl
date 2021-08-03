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

-export([start_battery/1]).

start_battery(Name) ->
	put(baterry_lvl,100),
	batteryMode(100.0,sleep,Name).

batteryMode(Battery_lvl,_,Sensor_ID) when Battery_lvl =< 0 ->
	%Battery: Battery level 0% Power Off
	sensor:power_off(Sensor_ID);
batteryMode(Battery_lvl,awake,Sensor_ID) ->
	New_Battery_lvl = battery_activity(Battery_lvl,300,3),				%awake for 3 s before sendig data and going back to sleep
	sensor:gotoSleep(Sensor_ID),
	batteryMode(New_Battery_lvl,sleep,Sensor_ID);

batteryMode(Battery_lvl,sleep,Sensor_ID) ->
	New_Battery_lvl = battery_activity(Battery_lvl,900,1),				%sleep for 3 s before randomizing P
	%Battery: call sensor with request 'randomize_P'
	Next_state = sensor:randomize_P(Sensor_ID),
	case Next_state of
		sleep -> batteryMode(New_Battery_lvl,sleep,Sensor_ID);
		awake -> batteryMode(New_Battery_lvl - 0.2,awake,Sensor_ID)		% 2 precent penalty for monitoring data
	end.


battery_activity(Battery_lvl,_,0) ->	Battery_lvl;
battery_activity(Battery_lvl,Timeout,Battery_drop) ->
	timer:sleep(Timeout),
	put(baterry_lvl,trunc(math:ceil(Battery_lvl - 0.1))),
	battery_activity(Battery_lvl - 0.1, Timeout, Battery_drop - 1).





%% API