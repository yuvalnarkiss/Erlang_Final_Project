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

-export([batterySleep/1,batteryAwake/1]).
batterySleep(0) -> ok;
batterySleep(N) ->
  receive
    wakeUp -> batteryAwake(N)
  after 2000 -> batterySleep(N-1)
  end.

batteryAwake(0) -> ok;
batteryAwake(N) ->
  receive
    sleep -> batterySleep(N)
  after 1500 -> batteryAwake(N-1)
  end.



%% API