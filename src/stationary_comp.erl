%%%-------------------------------------------------------------------
%%% @author katrinn
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Aug 2021 19:36
%%%-------------------------------------------------------------------
-module(stationary_comp).
-author("katrinn").

-define(DATA_LIMIT,5).

%% API
-export([start_loop/0]).

start_loop() ->
  start_loop([]).

start_loop(Data) when length(Data) == ?DATA_LIMIT ->
  main_PC:transfer_data(Data),
  start_loop([]);
start_loop(Data) ->
  receive
    {data,New_Data} -> io:format("sc got data ~p ~n", [New_Data]), start_loop( New_Data ++ Data );
    stop -> ok
  end.