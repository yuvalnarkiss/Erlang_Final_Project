-module('main_PC').
-behaviour(gen_server).
%-include("params.hrl").


% ============ Exports ===========
-export([init/1,start_link/4,handle_call/3,handle_cast/2,handle_info/2,terminate/2,transfer_data/1,getETSdata/1,shutdown/0]).

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
start_link(PC1,PC2,PC3,PC4) ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [PC1,PC2,PC3,PC4], []).
% ============ Functions ===========
%BY POSITION

init([PC1,PC2,PC3,PC4]) ->
  %ets:new(totalData,[set]), *** Consider using ets for all the data together, because it will a duplication of an already existing data in the ets of quarters
  ets:new(ulQuarter,[bag,named_table]),
  ets:new(urQuarter,[bag,named_table]),
  ets:new(dlQuarter,[bag,named_table]),
  ets:new(drQuarter,[bag,named_table]),
  global:register_name(main_PC,self()),

  % connect all nodes
  net_kernel:monitor_nodes(true),
  timer:sleep(200),
  net_kernel:connect_node(PC1),
  timer:sleep(200),
  net_kernel:connect_node(PC2),
  timer:sleep(200),
  net_kernel:connect_node(PC3),
  timer:sleep(200),
  net_kernel:connect_node(PC4),
  timer:sleep(200),

  % Start graphics
  WXServerPid = graphic:start(),
  FullSensorList = receiveSensorList([],0),

  Pos_List = [POS|| {_PID,POS} <- FullSensorList],
  Radius = find_radius(Pos_List),
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
    {X,Y} when X < 480 , Y < 480 -> ulQuarter;
    {X,Y} when X > 479 , Y < 480 -> urQuarter;
    {X,Y} when X < 480 , Y > 479 -> dlQuarter;
    {X,Y} when X > 479 , Y > 479 -> drQuarter
  end.

update_sensor_data(Map) ->
  Sensor_Pos = maps:get(position,Map),
  case find_quarter(Sensor_Pos) of
    ulQuarter -> ets:insert(ulQuarter,{Sensor_Pos,maps:remove(position,Map)});
    urQuarter -> ets:insert(urQuarter,{Sensor_Pos,maps:remove(position,Map)});
    dlQuarter -> ets:insert(dlQuarter,{Sensor_Pos,maps:remove(position,Map)});
    drQuarter -> ets:insert(drQuarter,{Sensor_Pos,maps:remove(position,Map)})
  end.

%for a given quarter and a time filter that should be in minutes(i.e 5 minutes, if an hour then 60 minutes etc..), or the atom none for no filter. gives the required average data.
quarter_cumulative_averages(Quarter,AllPosList,TimeFilter) ->
  CurrTime = calendar:universal_time(),
  case Quarter of
    %Every case(every case is a quarter) returns a tuple which consists of the average of each data type.
    ulQuarter -> TempETS = ets:new(tempETS,[bag]),
      [insert_data_to_tempETS(accumulated_stat_of_sensor(ets:lookup(ulQuarter,POS),CurrTime,TimeFilter),TempETS)|| POS <- AllPosList],
      TempAVG = lists:sum(lists:flatten(ets:match(TempETS,{temp,'$1'})))/erlang:length(lists:flatten(ets:match(TempETS,{temp,'$1'}))),
      SelfTempAVG = lists:sum(lists:flatten(ets:match(TempETS,{self_temp,'$1'})))/erlang:length(lists:flatten(ets:match(TempETS,{self_temp,'$1'}))),
      HumidityAVG = lists:sum(lists:flatten(ets:match(TempETS,{humidity,'$1'})))/erlang:length(lists:flatten(ets:match(TempETS,{humidity,'$1'}))),
      ets:delete(TempETS),
      {TempAVG,SelfTempAVG,HumidityAVG};

    urQuarter -> TempETS = ets:new(tempETS,[bag]),
      [insert_data_to_tempETS(accumulated_stat_of_sensor(ets:lookup(urQuarter,POS),CurrTime,TimeFilter),TempETS)|| POS <- AllPosList],
      TempAVG = lists:sum(lists:flatten(ets:match(TempETS,{temp,'$1'})))/erlang:length(lists:flatten(ets:match(TempETS,{temp,'$1'}))),
      SelfTempAVG = lists:sum(lists:flatten(ets:match(TempETS,{self_temp,'$1'})))/erlang:length(lists:flatten(ets:match(TempETS,{self_temp,'$1'}))),
      HumidityAVG = lists:sum(lists:flatten(ets:match(TempETS,{humidity,'$1'})))/erlang:length(lists:flatten(ets:match(TempETS,{humidity,'$1'}))),
      ets:delete(TempETS),
      {TempAVG,SelfTempAVG,HumidityAVG};

    dlQuarter -> TempETS = ets:new(tempETS,[bag]),
      [insert_data_to_tempETS(accumulated_stat_of_sensor(ets:lookup(dlQuarter,POS),CurrTime,TimeFilter),TempETS)|| POS <- AllPosList],
      TempAVG = lists:sum(lists:flatten(ets:match(TempETS,{temp,'$1'})))/erlang:length(lists:flatten(ets:match(TempETS,{temp,'$1'}))),
      SelfTempAVG = lists:sum(lists:flatten(ets:match(TempETS,{self_temp,'$1'})))/erlang:length(lists:flatten(ets:match(TempETS,{self_temp,'$1'}))),
      HumidityAVG = lists:sum(lists:flatten(ets:match(TempETS,{humidity,'$1'})))/erlang:length(lists:flatten(ets:match(TempETS,{humidity,'$1'}))),
      ets:delete(TempETS),
      {TempAVG,SelfTempAVG,HumidityAVG};

    drQuarter -> TempETS = ets:new(tempETS,[bag]),
      [insert_data_to_tempETS(accumulated_stat_of_sensor(ets:lookup(drQuarter,POS),CurrTime,TimeFilter),TempETS)|| POS <- AllPosList],
      TempAVG = lists:sum(lists:flatten(ets:match(TempETS,{temp,'$1'})))/erlang:length(lists:flatten(ets:match(TempETS,{temp,'$1'}))),
      SelfTempAVG = lists:sum(lists:flatten(ets:match(TempETS,{self_temp,'$1'})))/erlang:length(lists:flatten(ets:match(TempETS,{self_temp,'$1'}))),
      HumidityAVG = lists:sum(lists:flatten(ets:match(TempETS,{humidity,'$1'})))/erlang:length(lists:flatten(ets:match(TempETS,{humidity,'$1'}))),
      ets:delete(TempETS),
      {TempAVG,SelfTempAVG,HumidityAVG}
  end.

getETSdata(ETS) ->
  case ETS of
    ulQuarter -> ets:match_object(ulQuarter, {'$0', '$1'});
    urQuarter -> ets:match_object(urQuarter, {'$0', '$1'});
    dlQuarter -> ets:match_object(dlQuarter, {'$0', '$1'});
    drQuarter -> ets:match_object(drQuarter, {'$0', '$1'})
  end.


receiveSensorList(AggregatedSensorList,4) -> AggregatedSensorList ++ [{{stationary_comp,spawn(stationary_comp,start_loop,[])},{940,0}}];
receiveSensorList(AggregatedSensorList,N) ->
  receive
    {sens_list,ReceivedSensorList} ->  receiveSensorList(AggregatedSensorList ++ ReceivedSensorList,N+1);
    _-> receiveSensorList(AggregatedSensorList,N)     % in case of receiving wrong message
  end.

%checkDist(G,{PID,POS},{PID1,POS1},Radius) ->
%  case dist(POS,POS1) < Radius of
%    true -> digraph:add_edge(G,PID,PID1);
%    false -> false
%  end.
checkDist(POS,POS1,Radius) ->
  case dist(POS,POS1) =< Radius of
    true -> true;
    false -> false
  end.

sendNeighbourList(Sensor_Name,NhbrList) ->
  sensor:update_neighbors(Sensor_Name,NhbrList).

accumulated_stat_of_sensor(ElemList,CurrTime,TimeFilter) ->
  TempETS = ets:new(tempETS,[bag]),
  [insert_map_to_tempETS(Map,TempETS,CurrTime,TimeFilter)||{_POS,Map} <- ElemList],
  TempAVG = lists:sum(lists:flatten(ets:match(TempETS,{temp,'$1'})))/erlang:length(lists:flatten(ets:match(TempETS,{temp,'$1'}))),
  SelfTempAVG = lists:sum(lists:flatten(ets:match(TempETS,{self_temp,'$1'})))/erlang:length(lists:flatten(ets:match(TempETS,{self_temp,'$1'}))),
  HumidityAVG = lists:sum(lists:flatten(ets:match(TempETS,{humidity,'$1'})))/erlang:length(lists:flatten(ets:match(TempETS,{humidity,'$1'}))),
  ets:delete(TempETS),
  {TempAVG,SelfTempAVG,HumidityAVG}.



insert_map_to_tempETS(Map,TempETS,CurrTime,TimeFilter) ->
   SensTimeStamp = maps:get(time,Map),
  case check_time(CurrTime,SensTimeStamp,TimeFilter) of
    true -> ets:insert(TempETS,{time,maps:get(time,Map)}),
            ets:insert(TempETS,{temp,maps:get(temp,Map)}),
            ets:insert(TempETS,{self_temp,maps:get(self_temp,Map)}),
            ets:insert(TempETS,{humidity,maps:get(humidity,Map)});
    false -> ok
  end.


insert_data_to_tempETS({TimeData,TempData,SelfTempData,HumidityData},TempETS) ->
  ets:insert(TempETS,{time,TimeData}),
  ets:insert(TempETS,{temp,TempData}),
  ets:insert(TempETS,{self_temp,SelfTempData}),
  ets:insert(TempETS,{humidity,HumidityData}).

check_time(_CurrTime,_SensorTime,none) -> true;
check_time(CurrTime,SensorTime,TimeFilter) -> %TimeFilter should be in minutes
  TimeDiff = calendar:datetime_to_gregorian_seconds(CurrTime) - calendar:datetime_to_gregorian_seconds(SensorTime),
  case TimeDiff =< 60*TimeFilter of
    true -> true;
    false -> false
  end.