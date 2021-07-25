%%%-------------------------------------------------------------------
%%% @author ubuntu
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jul 2021 5:08 PM
%%%-------------------------------------------------------------------
-module('Sensor').
-author("ubuntu").

-behaviour(gen_statem).

%% API
-export([start/0]).

%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,
  code_change/4, callback_mode/0]).

-define(SERVER, ?MODULE).

-record('Sensor'_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start(NhbrList) ->
  gen_statem:start({local}, ?MODULE, NhbrList, []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init(NhbrList) ->
  %randomize position of sensor in matrix
  %start battery fsm with 100%
  BatteryPID = spawn_link(battery,batterySleep,[100]),
  {ok, sleep, {BatteryPID,NhbrList}}.

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

sleep(cast,)

state_name(_EventType, _EventContent, State = #'Sensor'_state{}) ->
NextStateName = next_state,
{next_state, NextStateName, State}.

%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
handle_event(_EventType, _EventContent, _StateName, State = #'Sensor'_state{}) ->
NextStateName = the_next_state_name,
{next_state, NextStateName, State}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _State = #'Sensor'_state{}) ->
ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #'Sensor'_state{}, _Extra) ->
{ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
battery