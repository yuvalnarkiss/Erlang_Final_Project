-module('main_PC').
-behaviour(gen_server).
%-include("params.hrl").


% ============ Exports ===========
-export([init/1,start_link/0,handle_call/3,handle_cast/2,handle_info/2,terminate/2,transfer_data/1,getETSdata/1,shutdown/0]).

-record(state,{nodes,wxPid}).
%-record('Sensor', {}).

%%%===================================================================
%%% API
%%%===================================================================

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================



transfer_data(Data) ->
  gen_server:cast(main_PC,{transfer_data,Data}).

shutdown()->
  gen_server:stop().


%% @doc Spawns the server and registers the local name (unique)
start_link() ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).
% ============ Functions ===========
%BY POSITION

init([]) ->
  %ets:new(totalData,[set]), *** Consider using ets for all the data together, because it will a duplication of an already existing data in the ets of quarters
  ets:new(ulQuarter,[bag]),
  ets:new(urQuarter,[bag]),
  ets:new(dlQuarter,[bag]),
  ets:new(drQuarter,[bag]),
  global:register_name(main_PC,self()),
  net_kernel:monitor_nodes(true),
  WXServerPid = graphic:start(),
  FullSensorList = receiveSensorList([],0),
  %Graph = digraph:new(),
  Pos_List = [POS|| {_PID,POS} <- FullSensorList],
  Radius = find_radius(Pos_List),
  %[digraph:add_vertex(Graph,PID,{PID,POS}) || {PID,POS} <- FullSensorList],
  %[[checkDist(Graph,{PID,POS},{PID1,POS1},Radius) || {PID1,POS1} <- FullSensorList] || {PID,POS} <- FullSensorList],
  [sendNeighbourList(PID,[{PID1,POS1}|| {PID1,POS1} <- FullSensorList,checkDist(POS,POS1,Radius)]) || {PID,POS} <- FullSensorList],

  {ok, #state{nodes =[],wxPid=WXServerPid}}.


handle_cast({transfer_data,ListOfDatas},State) ->
  [update_sensor_data(Map)||  Map <-ListOfDatas],
  {noreply,State}.



handle_call(example, _From, State) ->
  {reply,ok, State}.


handle_info({nodedown, _Node}, State) ->
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

terminate(_Reason, _State) ->
  ets:delete(ulQuarter),ets:delete(urQuarter),ets:delete(dlQuarter),ets:delete(drQuarter),
  ok.

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

find_quarter(Sensor_Pos) ->
  case Sensor_Pos of
    {X,Y} when X < 490 , Y < 490 -> ulQuarter;
    {X,Y} when X > 489 , Y < 490 -> urQuarter;
    {X,Y} when X < 490 , Y > 489 -> dlQuarter;
    {X,Y} when X > 489 , Y > 489 -> drQuarter
  end.

update_sensor_data(Map) ->
  Sensor_Pos = maps:get(position,Map),
  case find_quarter(Sensor_Pos) of
    ulQuarter -> ets:insert(ulQuarter,{Sensor_Pos,maps:remove(position,Map)});
    urQuarter -> ets:insert(urQuarter,{Sensor_Pos,maps:remove(position,Map)});
    dlQuarter -> ets:insert(dlQuarter,{Sensor_Pos,maps:remove(position,Map)});
    drQuarter -> ets:insert(drQuarter,{Sensor_Pos,maps:remove(position,Map)})
  end.

quarter_current_averages(Quarter,AllPosList) ->
  case Quarter of
    %Every case(every case is a quarter) returns a tuple which consists of the average of each data type.
    ulQuarter -> AllDataList =  [{Data1,Data2,Data3,Data4}|| #{ time := Data4,temp := Data2,self_temp := Data3,humidity := Data1} <- ets:match(ulQuarter,{'_','$1'})],
      Data4List = [D1 || {D1,_,_,_} <-AllDataList], Data2List = [D2 || {_,D2,_,_} <-AllDataList],  Data3List = [D3 || {_,_,D3,_} <-AllDataList],
      {lists:sum(Data4List)/length(Data4List),lists:sum(Data2List)/length(Data2List),lists:sum(Data3List)/length(Data3List)};

    %ulQuarter -> LIST = [|| {} <- AllPosList]




    urQuarter -> AllDataList =  [{Data1,Data2,Data3,Data4}|| #{ time := Data4,temp := Data2,self_temp := Data3,humidity := Data1} <- ets:match(urQuarter,{'_','$1'})],
      Data4List = [D1 || {D1,_,_,_} <-AllDataList], Data2List = [D2 || {_,D2,_,_} <-AllDataList],  Data3List = [D3 || {_,_,D3,_} <-AllDataList],
      {lists:sum(Data4List)/length(Data4List),lists:sum(Data2List)/length(Data2List),lists:sum(Data3List)/length(Data3List)};
    dlQuarter -> AllDataList =  [{Data1,Data2,Data3,Data4}|| #{ time := Data4,temp := Data2,self_temp := Data3,humidity := Data1} <- ets:match(dlQuarter,{'_','$1'})],
      Data4List = [D1 || {D1,_,_,_} <-AllDataList], Data2List = [D2 || {_,D2,_,_} <-AllDataList],  Data3List = [D3 || {_,_,D3,_} <-AllDataList],
      {lists:sum(Data4List)/length(Data4List),lists:sum(Data2List)/length(Data2List),lists:sum(Data3List)/length(Data3List)};
    drQuarter -> AllDataList =  [{Data1,Data2,Data3,Data4}|| #{ time := Data4,temp := Data2,self_temp := Data3,humidity := Data1} <- ets:match(drQuarter,{'_','$1'})],
      Data4List = [D1 || {D1,_,_,_} <-AllDataList], Data2List = [D2 || {_,D2,_,_} <-AllDataList],  Data3List = [D3 || {_,_,D3,_} <-AllDataList],
      {lists:sum(Data4List)/length(Data4List),lists:sum(Data2List)/length(Data2List),lists:sum(Data3List)/length(Data3List)}
  end.

getETSdata(ETS) ->
  case ETS of
    ulQuarter -> ets:match_object(ulQuarter, {'$0', '$1'});
    urQuarter -> ets:match_object(urQuarter, {'$0', '$1'});
    dlQuarter -> ets:match_object(dlQuarter, {'$0', '$1'});
    drQuarter -> ets:match_object(drQuarter, {'$0', '$1'})
  end.


receiveSensorList(AggregatedSensorList,4) -> AggregatedSensorList ++ [{spawn(stationary_comp,start_loop,[]),{940,0}}];
receiveSensorList(AggregatedSensorList,N) ->
  receive
    ReceivedSensorList ->  receiveSensorList(AggregatedSensorList ++ ReceivedSensorList,N+1)
  end.

%checkDist(G,{PID,POS},{PID1,POS1},Radius) ->
%  case dist(POS,POS1) < Radius of
%    true -> digraph:add_edge(G,PID,PID1);
%    false -> false
%  end.
checkDist(POS,POS1,Radius) ->
  case dist(POS,POS1) < Radius of
    true -> true;
    false -> false
  end.

sendNeighbourList(Sensor_Name,NhbrList) ->
  sensor:update_neighbors(Sensor_Name,NhbrList).
