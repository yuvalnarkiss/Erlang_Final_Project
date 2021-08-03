-module('main_PC').
-behaviour(gen_object).
%-include("params.hrl").


% ============ Exports ===========
-export([init/1]).
-export([start_link/0]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-record(state,{nodes,wxPid}).

%%%===================================================================
%%% API
%%%===================================================================
%% @doc Spawns the server and registers the local name (unique)
start_link() ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).
% ============ Functions ===========

init([]) ->
  global:register_name(main_PC,self()),
  WXServerPid = wx_server:start(),
  {ok, #state{nodes =[],wxPid=WXServerPid}}.



handle_cast(example,State) ->
  {noreply,State}.

handle_call(example, From, State) ->
  {reply,ok, State}.


handle_info({nodedown, Node}, State) ->
  io:fwrite("node down!~n"),
  %NumOfServers = get(numOfServers),
  %put(numOfServers,NumOfServers-1),
  %PrevServer = cancelNode(Node),
  %NewServer = findFreeNode(),
  %case NewServer of
  %  none -> io:fwrite("all nodes are crushed~n");
  %  _ ->put(PrevServer, NewServer), %connect the prev server to the node server origin
  %    slave_server:crushControl(NewServer ,getScreenList(NewServer) ++ getScreenList(PrevServer) ,ets:tab2list(PrevServer)),
  %    ets:delete_all_objects(PrevServer)
  %end,
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

find_radius([{_X,_Y}]) -> 0;
find_radius([{X1,Y1}| Pos_list]) ->
  Dist = [ dist({X1,Y1},{X2,Y2}) || {X2, Y2} <- Pos_list],
  Min_dist = lists:min(Dist),
  Min_Radius = lists:max([ Min_dist | find_radius(Pos_list) ]),
  Min_Radius.

dist({X1,Y1},{X2,Y2}) -> trunc(math:ceil(math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)))).