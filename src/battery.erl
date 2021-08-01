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

-export([batteryMode/3]).
batteryMode(Battery_lvl,_,Sensor_ID) when Battery_lvl =< 0 ->
	%io:format("Battery: Battery level 0% Power Off ~n", []), %ToDo:Temp comment
	sensor:power_off(Sensor_ID);
batteryMode(Battery_lvl,awake,Sensor_ID) ->
	%io:format("Battery mode: awake ,Battery level: ~p ~n", [Battery_lvl]), %ToDo:Temp comment
	New_Battery_lvl = battery_activity(Battery_lvl,1500,3),				%awake for 4.5 ms before sendig data and going back to sleep
	%io:format("Battery: call sensor with request 'gotoSleep' ~n", []), %ToDo:Temp comment
	sensor:gotoSleep(Sensor_ID),
	batteryMode(New_Battery_lvl,sleep,Sensor_ID);

batteryMode(Battery_lvl,sleep,Sensor_ID) ->
	%io:format("Battery mode: sleep ,Battery level: ~p ~n", [Battery_lvl]), %ToDo:Temp comment
	New_Battery_lvl = battery_activity(Battery_lvl,2000,2),				%sleep for 4 ms before randomizing P
	%io:format("Battery: call sensor with request 'randomize_P' ~n", []), %ToDo:Temp comment
	Next_state = sensor:randomize_P(Sensor_ID),
	%io:format("Battery: next state ~p ~n", [Next_state]), %ToDo:Temp comment
	case Next_state of
		sleep -> batteryMode(New_Battery_lvl,sleep,Sensor_ID);
		awake -> batteryMode(New_Battery_lvl-2,awake,Sensor_ID)		% 2 precent penalty for monitoring data
	end.


battery_activity(Battery_lvl,_,0) ->	Battery_lvl;
battery_activity(Battery_lvl,Timeout,N) ->
	timer:sleep(Timeout),
	battery_activity(Battery_lvl - 1, Timeout, N - 1).





%% API