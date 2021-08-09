-module('main_PC').
-behaviour(gen_server).
%-include("params.hrl").

-define(ULQXBOUNDARY, 480).
-define(ULQYBOUNDARY, 440).
-define(URQXBOUNDARY, 460).
-define(URQYBOUNDARY, 440).
-define(DLQXBOUNDARY, 480).
-define(DLQYBOUNDARY, 420).
-define(DRQXBOUNDARY, 460).
-define(DRQYBOUNDARY, 420).
% ============ Exports ===========
-export([init/1,start_link/4,handle_call/3,handle_cast/2,handle_info/2,terminate/2,transfer_data/1,getETSdata/1,shutdown/0,periodic_sensor_status_update/1]).

-record(state,{nodes,wxPid}).
%-record('Sensor', {}).

%%%===================================================================
%%% API
%%%===================================================================

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

periodic_sensor_status_update(ETSList) ->
  gen_server:cast(?MODULE,{periodic_sensor_status_update,ETSList}).


transfer_data(Data) ->
  gen_server:cast(?MODULE,{transfer_data,Data}).

shutdown()->
  gen_server:stop().


%% @doc Spawns the server and registers the local name (unique)
start_link(PC1,PC2,PC3,PC4) ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [PC1,PC2,PC3,PC4], []).
% ============ Functions ===========
%BY POSITION


init([PC1,PC2,PC3,PC4]) ->
  ets:new(ulQuarter,[bag,named_table]),
  ets:new(ulQuarterStatus,[set,named_table]),
  ets:new(urQuarter,[bag,named_table]),
  ets:new(urQuarterStatus,[set,named_table]),
  ets:new(dlQuarter,[bag,named_table]),
  ets:new(dlQuarterStatus,[set,named_table]),
  ets:new(drQuarter,[bag,named_table]),
  ets:new(drQuarterStatus,[set,named_table]),
  ets:new(positionsByQuarter,[bag,named_table]),
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
  create_quarter_positions_list(Pos_List),
  Radius = find_radius(Pos_List),
  [sendNeighbourList(PID,[{PID1,POS1}|| {PID1,POS1} <- FullSensorList,checkDist(POS,POS1,Radius)]) || {PID,POS} <- FullSensorList],
  {ok, #state{nodes =[],wxPid=WXServerPid}}.


handle_cast({transfer_data,ListOfDatas},State) ->
  [update_sensor_data(Map,map)||  Map <-ListOfDatas],
  %{TempAVG,SelfTempAVG,HumidityAVG};
  {ULTempAVG,ULSelfTempAVG,ULHumidityAVG} = quarter_cumulative_averages(ulQuarter,lists:flatten(ets:match(positionsByQuarter,{ulQuarter,'$1'})),100),
  {URTempAVG,URSelfTempAVG,URHumidityAVG} = quarter_cumulative_averages(urQuarter,lists:flatten(ets:match(positionsByQuarter,{urQuarter,'$1'})),100),
  {DLTempAVG,DLSelfTempAVG,DLHumidityAVG} = quarter_cumulative_averages(dlQuarter,lists:flatten(ets:match(positionsByQuarter,{dlQuarter,'$1'})),100),
  {DRTempAVG,DRSelfTempAVG,DRHumidityAVG} = quarter_cumulative_averages(drQuarter,lists:flatten(ets:match(positionsByQuarter,{drQuarter,'$1'})),100),
  WholeTempAVG = (ULTempAVG+URTempAVG+DLTempAVG+DRTempAVG)/4,
  WholeSelfTempAVG = (ULSelfTempAVG+URSelfTempAVG+DLSelfTempAVG+DRSelfTempAVG)/4,
  WholeHumidityAVG = (ULHumidityAVG+URHumidityAVG+DLHumidityAVG+DRHumidityAVG)/4,
  graphic:update_status(self_temp,WholeSelfTempAVG),
  graphic:update_status(ulQuarter,{ULTempAVG,ULHumidityAVG}),
  graphic:update_status(urQuarter,{URTempAVG,URHumidityAVG}),
  graphic:update_status(dlQuarter,{DLTempAVG,DLHumidityAVG}),
  graphic:update_status(drQuarter,{DRTempAVG,DRHumidityAVG}),
  graphic:update_status(whole,{WholeTempAVG,WholeHumidityAVG}),

  %update graphic for each quarter
  {noreply,State}.

handle_cast({periodic_sensor_status_update,ETSList},State) ->
  update_sensor_data(ETSList,ets),
  {noreply,State}.


handle_call(example, _From, State) ->
  {reply,ok, State}.

handle_info({'ETS-TRANSFER', TableId, _OldOwner, _GiftData}, State) ->
  ETS_old_list = ets:tab2list(TableId),
  recreate_sensors(ETS_old_list),
  {noreply, State};

handle_info({nodedown, Node}, State) ->
  case Node of
    pc1 ->
      PC2Ping = net_adm:ping('pc2'),
      PC3Ping = net_adm:ping('pc3'),
      PC4Ping = net_adm:ping('pc4'),
      ResponseList = [PC2Ping,PC3Ping,PC4Ping],
      case ResponseList of
        [pong,_,_] -> rpc:call(pc2,server,funcname,[ets:tab2list(ulQuarterStatus)]);
        [pang,pong,_] ->rpc:call(pc3,server,funcname,[ets:tab2list(ulQuarterStatus)]); %give reasponsibility of pc1 to pc3(this scenario means that pc1 is down, and ping check to pc2 was bad and ping check to pc3 was good).
        [pang,pang,pong] ->rpc:call(pc4,server,funcname,[ets:tab2list(ulQuarterStatus)]) %give responsibility of pc1 to pc4
      end;
    pc2 ->
      PC1Ping = net_adm:ping('pc1'),
      PC3Ping = net_adm:ping('pc3'),
      PC4Ping = net_adm:ping('pc4'),
      ResponseList = [PC1Ping,PC3Ping,PC4Ping],
      case ResponseList of
        [pong,_,_] -> rpc:call(pc1,server,funcname,[ets:tab2list(urQuarterStatus)]);
        [pang,pong,_] ->rpc:call(pc3,server,funcname,[ets:tab2list(urQuarterStatus)]); %give reasponsibility of pc1 to pc3(this scenario means that pc1 is down, and ping check to pc2 was bad and ping check to pc3 was good).
        [pang,pang,pong] ->rpc:call(pc4,server,funcname,[ets:tab2list(urQuarterStatus)]) %give responsibility of pc1 to pc4
      end;
    pc3->
      PC1Ping = net_adm:ping('pc1'),
      PC2Ping = net_adm:ping('pc2'),
      PC4Ping = net_adm:ping('pc4'),
      ResponseList = [PC1Ping,PC2Ping,PC4Ping],
      case ResponseList of
        [pong,_,_] ->rpc:call(pc1,server,funcname,[ets:tab2list(dlQuarterStatus)]); %give responsibility of pc3 to pc1
        [pang,pong,_] ->rpc:call(pc2,server,funcname,[ets:tab2list(dlQuarterStatus)]); %give responsibility of pc3 to pc2(this scenario means that pc3 is down, and ping check to pc1 was bad and ping check to pc2 was good).
        [pang,pang,pong] ->rpc:call(pc4,server,funcname,[ets:tab2list(dlQuarterStatus)]) %give responsibility of pc3 to pc4
      end;
    pc4 ->
      PC1Ping = net_adm:ping('pc1'),
      PC2Ping = net_adm:ping('pc2'),
      PC3Ping = net_adm:ping('pc3'),
      ResponseList = [PC1Ping,PC2Ping,PC3Ping],
      case ResponseList of
        [pong,_,_] ->rpc:call(pc1,server,funcname,[ets:tab2list(drQuarterStatus)]); %give responsibility of pc4 to pc1
        [pang,pong,_] ->rpc:call(pc2,server,funcname,[ets:tab2list(drQuarterStatus)]); %give responsibility of pc4 to pc2(this scenario means that pc4 is down, and ping check to pc1 was bad and ping check to pc2 was good).
        [pang,pang,pong] ->rpc:call(pc3,server,funcname,[ets:tab2list(drQuarterStatus)]) %give responsibility of pc4 to pc3
      end
  end,
  {noreply, State}.

%handle_info({nodedown, PC1}, State) ->
  %io:fwrite("node down!~n"),
  %PC2Ping = net_adm:ping('PC2'),
  %PC3Ping = net_adm:ping('PC3'),
  %PC4Ping = net_adm:ping('PC4'),
  %ResponseList = [PC2Ping,PC3Ping,PC4Ping],
  %case ResponseList of
    %[pong,_,_] -> _;%give responsibility of pc1 to pc2
      %              %PC2!ets:tab2list(ulQuarterStatus)
     %               rpc:call(NODE,server,funcname,[ets:tab2list(ulQuarterStatus)])
    %[pang,pong,_] ->_; %give reasponsibility of pc1 to pc3(this scenario means that pc1 is down, and ping check to pc2 was bad and ping check to pc3 was good).
   % [pang,pang,pong] ->_ %give responsibility of pc1 to pc4
 % end,
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

terminate(_Reason, _State) ->
  ets:delete(ulQuarter),ets:delete(urQuarter),ets:delete(dlQuarter),ets:delete(drQuarter),
  ets:delete(ulQuarterStatus),ets:delete(urQuarterStatus),ets:delete(dlQuarterStatus),ets:delete(drQuarterStatus),
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
    {X,Y} when X < ?ULQXBOUNDARY , Y < ?ULQYBOUNDARY -> ulQuarter;
    {X,Y} when X > ?URQXBOUNDARY , Y < ?URQYBOUNDARY -> urQuarter;
    {X,Y} when X < ?DLQXBOUNDARY , Y > ?DLQYBOUNDARY -> dlQuarter;
    {X,Y} when X > ?DRQXBOUNDARY , Y > ?DRQYBOUNDARY -> drQuarter
  end.

update_sensor_data(Map,map) ->
  Sensor_Pos = maps:get(position,Map),
  case find_quarter(Sensor_Pos) of
    ulQuarter -> ets:insert(ulQuarter,{Sensor_Pos,maps:remove(position,Map)});
    urQuarter -> ets:insert(urQuarter,{Sensor_Pos,maps:remove(position,Map)});
    dlQuarter -> ets:insert(dlQuarter,{Sensor_Pos,maps:remove(position,Map)});
    drQuarter -> ets:insert(drQuarter,{Sensor_Pos,maps:remove(position,Map)})
  end;

update_sensor_data(ETSList,ets) ->
  {POS,_} = lists:nth(1,ETSList),
  case find_quarter(POS) of
    ulQuarter -> ets:insert(ulQuarterStatus,ETSList);
    urQuarter -> ets:insert(urQuarterStatus,ETSList);
    dlQuarter -> ets:insert(dlQuarterStatus,ETSList);
    drQuarter -> ets:insert(drQuarterStatus,ETSList)
  end.

%for a given quarter and a time filter that should be in seconds or the atom none for no filter. gives the required average data.
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

accumulated_stat_of_sensor([],_,_) -> none;
accumulated_stat_of_sensor(ElemList,CurrTime,TimeFilter) ->
  TempETS = ets:new(tempETS,[bag]),
  [insert_map_to_tempETS(Map,TempETS,CurrTime,TimeFilter)||{_POS,Map} <- ElemList],
  TempAVG = lists:sum(lists:flatten(ets:match(TempETS,{temp,'$1'})))/erlang:length(lists:flatten(ets:match(TempETS,{temp,'$1'}))),
  SelfTempAVG = lists:sum(lists:flatten(ets:match(TempETS,{self_temp,'$1'})))/erlang:length(lists:flatten(ets:match(TempETS,{self_temp,'$1'}))),
  HumidityAVG = lists:sum(lists:flatten(ets:match(TempETS,{humidity,'$1'})))/erlang:length(lists:flatten(ets:match(TempETS,{humidity,'$1'}))),
  ets:delete(TempETS),
  {TempAVG,SelfTempAVG,HumidityAVG}.


insert_map_to_tempETS(none,_,_,_) -> none;
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
check_time(CurrTime,SensorTime,TimeFilter) -> %TimeFilter should be in seconds
  TimeDiff = calendar:datetime_to_gregorian_seconds(CurrTime) - calendar:datetime_to_gregorian_seconds(SensorTime),
  case TimeDiff =< TimeFilter of
    true -> true;
    false -> false
  end.

create_quarter_positions_list([]) -> [];
create_quarter_positions_list([H|T]) ->
  case find_quarter(H) of
    ulQuarter -> ets:insert(positionsByQuarter,{ulQuarter,H}),create_quarter_positions_list(T);
    urQuarter -> ets:insert(positionsByQuarter,{urQuarter,H}),create_quarter_positions_list(T);
    dlQuarter -> ets:insert(positionsByQuarter,{dlQuarter,H}),create_quarter_positions_list(T);
    drQuarter -> ets:insert(positionsByQuarter,{drQuarter,H}),create_quarter_positions_list(T)
  end.
