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
  register(stationary_comp,self()),
  start_loop([]).

start_loop(Data) when length(Data) == ?DATA_LIMIT ->
  %ToDo: add main_PC:transfer_data(Data) function to main_PC,  should be implemented with call to assure the data was safely transferred
  start_loop([]);
start_loop(Data) ->
  receive
    {data,New_Data} -> start_loop([ New_Data | Data ]);
    stop -> ok
  end.