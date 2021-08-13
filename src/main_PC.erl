-module('main_PC').
-behaviour(gen_server).
-include("params.hrl").

% ============ Exports ===========
-export([init/1,start_link/4,handle_call/3,handle_cast/2,handle_info/2,terminate/2,transfer_data/1,getETSdata/1,shutdown/0,periodic_sensor_status_update/2]).

-record(state,{nodes,wxPid,info_count,sensor_pos_list}).
%-record('Sensor', {}).

%%%===================================================================
%%% API
%%%===================================================================

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

periodic_sensor_status_update(ETSList,Node) ->
  gen_server:cast(?MODULE,{periodic_sensor_status_update,ETSList,Node}).


transfer_data(Data) ->
  gen_server:cast(?MODULE,{transfer_data,Data}).

shutdown()->
  gen_server:stop(?MODULE).


%% @doc Spawns the server and registers the local name (unique)
start_link(PC1,PC2,PC3,PC4) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [PC1,PC2,PC3,PC4], []).
% ============ Functions ===========
%BY POSITION


init([PC1,PC2,PC3,PC4]) ->

  % ets for statistics
  ets:new(ulQuarter,[bag,named_table]),
  ets:new(urQuarter,[bag,named_table]),
  ets:new(dlQuarter,[bag,named_table]),
  ets:new(drQuarter,[bag,named_table]),

  %ets for backup
  ets:new(pc3Backup,[set,named_table]),
  ets:new(pc2Backup,[set,named_table]),
  ets:new(pc1Backup,[set,named_table]),
  ets:new(pc4Backup,[set,named_table]),

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

  %start servers
  rpc:call(PC1,server,start_link,[node(),[PC1,PC2,PC3,PC4],pc1]),
  rpc:call(PC2,server,start_link,[node(),[PC1,PC2,PC3,PC4],pc2]),
  rpc:call(PC3,server,start_link,[node(),[PC1,PC2,PC3,PC4],pc3]),
  rpc:call(PC4,server,start_link,[node(),[PC1,PC2,PC3,PC4],pc4]),

  {ok, #state{nodes =[PC1,PC2,PC3,PC4],wxPid=WXServerPid,info_count=0,sensor_pos_list=[]}}.


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
  graphic:update_status(self_temp,WholeSelfTempAVG), %in graphics, this information will be updated in the status_db ets. key is self_temp, value is WholeSelfTempAVG
  graphic:update_status(q1,{ULTempAVG,ULHumidityAVG}),%in graphics, this information will be updated in the status_db ets. key is ulQuarter, value is the tuple {ULTempAVG,ULHumidityAVG}.
  graphic:update_status(q2,{URTempAVG,URHumidityAVG}), %Likewise.
  graphic:update_status(q3,{DLTempAVG,DLHumidityAVG}),%Likewise.
  graphic:update_status(q4,{DRTempAVG,DRHumidityAVG}),%Likewise.
  graphic:update_status(whole,{WholeTempAVG,WholeHumidityAVG}),%Likewise.
  %update graphic for each quarter
  {noreply,State};

handle_cast({periodic_sensor_status_update,ETSList,Node},#state{nodes = PCList} = State) ->
  update_sensor_status(ETSList,Node,PCList,ets),
  {noreply,State};
handle_cast({example,_ETSList},State) ->
  {noreply,State}.


handle_call(example, _From, State) ->
  {reply,ok, State}.


handle_info({sens_list,ReceivedSensorList}, #state{nodes = Node_list, info_count = Count, sensor_pos_list = SensorList} = State) ->
  case Count+1 of
    4 ->
      FullSensorList =	[{spawn(stationary_comp, start_loop,[]),?STATIONARY_COMP_POS}] ++ SensorList ++ ReceivedSensorList,
      Pos_List = [POS|| {_PID,POS} <- FullSensorList],
      create_quarter_positions_list(Pos_List),
      Radius = hd(find_radius(Pos_List)),
      [sendNeighbourList(Node_list,PID,POS,[{PID1,POS1}|| {PID1,POS1} <- FullSensorList,checkDist(POS,POS1,Radius)]) || {PID,POS} <- FullSensorList];
    _ -> ok
  end,
  {noreply, State#state{sensor_pos_list = SensorList ++ ReceivedSensorList,info_count = (Count+1)}};
handle_info({nodedown, Node}, #state{nodes =[PC1,PC2,PC3,PC4]} = State) ->
  case Node of
    PC1 ->
      PC2Ping = net_adm:ping(PC2),
      PC3Ping = net_adm:ping(PC3),
      PC4Ping = net_adm:ping(PC4),
      ResponseList = [PC2Ping,PC3Ping,PC4Ping],
      New_PC1 = case ResponseList of
                  [pong,_,_] -> PC2;
                  [pang,pong,_] -> PC3; %give reasponsibility of pc1 to pc3(this scenario means that pc1 is down, and ping check to pc2 was bad and ping check to pc3 was good).
                  [pang,pang,pong] -> PC4  %give responsibility of pc1 to pc4
                end,
      rpc:call(New_PC1,server,mergeETS,[ets:tab2list(pc1Backup)]),
      [ rpc:call(PC,ets,insert,[nodes,{pc1,New_PC1}]) || PC <- ResponseList, PC == pong];
    PC2 ->
      PC1Ping = net_adm:ping(PC1),
      PC3Ping = net_adm:ping(PC3),
      PC4Ping = net_adm:ping(PC4),
      ResponseList = [PC1Ping,PC3Ping,PC4Ping],
      New_PC2 = case ResponseList of
                  [pong,_,_] -> PC1;
                  [pang,pong,_] -> PC3; %give reasponsibility of pc1 to pc3(this scenario means that pc1 is down, and ping check to pc2 was bad and ping check to pc3 was good).
                  [pang,pang,pong] -> PC4  %give responsibility of pc1 to pc4
                end,
      rpc:call(New_PC2,server,mergeETS,[ets:tab2list(pc2Backup)]),
      [ rpc:call(PC,ets,insert,[nodes,{pc2,New_PC2}]) || PC <- ResponseList, PC == pong];
    PC3->
      PC1Ping = net_adm:ping(PC1),
      PC2Ping = net_adm:ping(PC2),
      PC4Ping = net_adm:ping(PC4),
      ResponseList = [PC1Ping,PC2Ping,PC4Ping],
      New_PC3 = case ResponseList of
                  [pong,_,_] -> PC1;
                  [pang,pong,_] -> PC2; %give reasponsibility of pc1 to pc3(this scenario means that pc1 is down, and ping check to pc2 was bad and ping check to pc3 was good).
                  [pang,pang,pong] -> PC4  %give responsibility of pc1 to pc4
                end,
      rpc:call(New_PC3,server,mergeETS,[ets:tab2list(pc3Backup)]),
      [ rpc:call(PC,ets,insert,[nodes,{pc3,New_PC3}]) || PC <- ResponseList, PC == pong];
    PC4 ->
      PC1Ping = net_adm:ping(PC1),
      PC2Ping = net_adm:ping(PC2),
      PC3Ping = net_adm:ping(PC3),
      ResponseList = [PC1Ping,PC2Ping,PC3Ping],
      New_PC4 = case ResponseList of
                  [pong,_,_] -> PC1;
                  [pang,pong,_] -> PC2; %give reasponsibility of pc1 to pc3(this scenario means that pc1 is down, and ping check to pc2 was bad and ping check to pc3 was good).
                  [pang,pang,pong] -> PC3  %give responsibility of pc1 to pc4
                end,
      rpc:call(New_PC4,server,mergeETS,[ets:tab2list(pc4Backup)]),
      [ rpc:call(PC,ets,insert,[nodes,{pc4,New_PC4}]) || PC <- ResponseList, PC == pong]
  end,
  {noreply, State};
handle_info(_Request, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  Connected_PCs = nodes(),
  [rpc:call(PC,server,shutdown,[]) || PC <- Connected_PCs ],
  ets:delete(ulQuarter),ets:delete(urQuarter),ets:delete(dlQuarter),ets:delete(drQuarter),
  ets:delete(pc1Backup),ets:delete(pc2Backup),ets:delete(pc3Backup),ets:delete(pc4Backup),
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

find_radius([{_X,_Y}]) -> [0];
find_radius([{X1,Y1}| Pos_list]) ->
  Dist = [ dist({X1,Y1},{X2,Y2}) || {X2, Y2} <- Pos_list],
  Min_dist = lists:min(Dist),
  Min_Radius = lists:max([ Min_dist | find_radius(Pos_list) ]),
  [Min_Radius].

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
  end.

update_sensor_status(ETSList,Node,[PC1,PC2,PC3,PC4],ets) ->
  case Node of
    PC1 -> ets:insert(pc1Backup,ETSList);
    PC2 -> ets:insert(pc2Backup,ETSList);
    PC3 -> ets:insert(pc3Backup,ETSList);
    PC4 -> ets:insert(pc4Backup,ETSList)
  end.

%for a given quarter and a time filter that should be in seconds or the atom none for no filter. gives the required average data.
quarter_cumulative_averages(Quarter,AllPosList,TimeFilter) ->
  CurrTime = calendar:universal_time(),
  %Every case(every case is a quarter) returns a tuple which consists of the average of each data type.
  TempETS = ets:new(tempETS,[bag]),
  [insert_data_to_tempETS(accumulated_stat_of_sensor(ets:lookup(Quarter,POS),CurrTime,TimeFilter),TempETS)|| POS <- AllPosList],
  TempAVG = avg_calc(TempETS,temp),
  SelfTempAVG = avg_calc(TempETS,self_temp),
  HumidityAVG = avg_calc(TempETS,humidity),
  ets:delete(TempETS),
  {TempAVG,SelfTempAVG,HumidityAVG}.

getETSdata(ETS) ->
  case ETS of
    ulQuarter -> ets:match_object(ulQuarter, {'$0', '$1'});
    urQuarter -> ets:match_object(urQuarter, {'$0', '$1'});
    dlQuarter -> ets:match_object(dlQuarter, {'$0', '$1'});
    drQuarter -> ets:match_object(drQuarter, {'$0', '$1'})
  end.

checkDist(POS,POS1,Radius) ->
  case dist(POS,POS1) =< Radius of
    true -> true;
    false -> false
  end.

sendNeighbourList([PC1,PC2,PC3,PC4],Sensor_Name,Sensor_Pos,NhbrList) ->
  Node = case find_quarter(Sensor_Pos) of
           ulQuarter -> PC1;
           urQuarter -> PC2;
           dlQuarter -> PC3;
           drQuarter -> PC4
         end,
  rpc:call(Node,sensor,update_neighbors,[Sensor_Name,NhbrList]).

accumulated_stat_of_sensor([],_,_) -> none;
accumulated_stat_of_sensor(ElemList,CurrTime,TimeFilter) ->
  TempETS = ets:new(tempETS,[bag]),
  [insert_map_to_tempETS(Map,TempETS,CurrTime,TimeFilter)||{_POS,Map} <- ElemList],
  TempAVG = avg_calc(TempETS,temp),
  SelfTempAVG = avg_calc(TempETS,self_temp),
  HumidityAVG = avg_calc(TempETS,humidity),
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

insert_data_to_tempETS(none,_) -> ok;
insert_data_to_tempETS({TempData,SelfTempData,HumidityData},TempETS) ->
  ets:insert(TempETS,{temp,TempData}),
  ets:insert(TempETS,{self_temp,SelfTempData}),
  ets:insert(TempETS,{humidity,HumidityData}).

check_time(_CurrTime,_SensorTime,none) -> true;
check_time(CurrTime,SensorTime,TimeFilter) -> %TimeFilter should be in miliseconds
  TimeDiff = calendar:datetime_to_gregorian_seconds(CurrTime) - calendar:datetime_to_gregorian_seconds(SensorTime),
  case TimeDiff/1000 =< TimeFilter/1000 of
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


avg_calc(TempETS,Key) ->
  Sum = lists:sum(lists:flatten(ets:match(TempETS,{Key,'$1'}))),
  Length = erlang:length(lists:flatten(ets:match(TempETS,{Key,'$1'}))),
  case Length of
    0 -> 0.0;
    _ -> Sum/Length
  end.