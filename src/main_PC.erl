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
-record('Sensor', {}).

%%%===================================================================
%%% API
%%%===================================================================

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

transfer_data(Data) ->
  gen_server:cast(main_PC,{transfer_data,Data}).

%% @doc Spawns the server and registers the local name (unique)
start_link() ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).
% ============ Functions ===========

init([]) ->
  %ets:new(totalData,[set]), *** Consider using ets for all the data together, because it will a duplication of an already existing data in the ets of quarters
  ets:new(ulQuarter,[set]),
  ets:new(urQuarter,[set]),
  ets:new(dlQuarter,[set]),
  ets:new(drQuarter,[set]),
  global:register_name(main_PC,self()),
  net_kernel:monitor_nodes(true),
  WXServerPid = graphic:start(),
  {ok, #state{nodes =[],wxPid=WXServerPid}}.


handle_cast({transfer_data,ListOfDatas},State) ->
  [update_sensor_data(Sensor_Pos,Data_List)|| #{position := Sensor_Pos, data_list := Data_List} <-ListOfDatas],
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

find_quarter(Sensor_Pos) ->
  case Sensor_Pos of
    {X,Y} when X < 490 and Y < 490 -> ulQuarter;
    {X,Y} when X > 489 and Y < 490 -> urQuarter;
    {X,Y} when X < 490 and Y > 489 -> dlQuarter;
    {X,Y} when X > 489 and Y > 489 -> drQuarter
  end.

update_sensor_data(Sensor_Pos,Data_List) ->
  case find_quarter(Sensor_Pos) of
    ulQuarter -> ets:insert(ulQuarter,{Sensor_Pos,Data_List});
    urQuarter -> ets:insert(urQuarter,{Sensor_Pos,Data_List});
    dlQuarter -> ets:insert(dlQuarter,{Sensor_Pos,Data_List});
    drQuarter -> ets:insert(drQuarter,{Sensor_Pos,Data_List})
  end.

quarter_statistics(Quarter) ->
  case Quarter of
    %ToDo: This is a structure of averaging all monitored data. data1,data2 etc.. needs to be replaced with to actual data name that we would monitor.
    %Every case(every case is a quarter) returns a tuple which consists of the average of each data type.
    ulQuarter -> AllDataList =  [{Data1,Data2,Data3}|| #{ data1 := Data1,data2 := Data2,data3 := Data3} <- ets:match(ulQuarter,{'_','$1'})],
      Data1List = [D1 || {D1,_,_} <-AllDataList], Data2List = [D2 || {_,D2,_} <-AllDataList],  Data3List = [D3 || {_,_,D3} <-AllDataList],
      {lists:sum(Data1List)/length(Data1List),lists:sum(Data2List)/length(Data2List),lists:sum(Data3List)/length(Data3List)};
    urQuarter -> AllDataList =  [{Data1,Data2,Data3}|| #{ data1 := Data1,data2 := Data2,data3 := Data3} <- ets:match(urQuarter,{'_','$1'})],
      Data1List = [D1 || {D1,_,_} <-AllDataList], Data2List = [D2 || {_,D2,_} <-AllDataList],  Data3List = [D3 || {_,_,D3} <-AllDataList],
      {lists:sum(Data1List)/length(Data1List),lists:sum(Data2List)/length(Data2List),lists:sum(Data3List)/length(Data3List)};
    dlQuarter -> AllDataList =  [{Data1,Data2,Data3}|| #{ data1 := Data1,data2 := Data2,data3 := Data3} <- ets:match(dlQuarter,{'_','$1'})],
      Data1List = [D1 || {D1,_,_} <-AllDataList], Data2List = [D2 || {_,D2,_} <-AllDataList],  Data3List = [D3 || {_,_,D3} <-AllDataList],
      {lists:sum(Data1List)/length(Data1List),lists:sum(Data2List)/length(Data2List),lists:sum(Data3List)/length(Data3List)};
    drQuarter -> AllDataList =  [{Data1,Data2,Data3}|| #{ data1 := Data1,data2 := Data2,data3 := Data3} <- ets:match(drQuarter,{'_','$1'})],
      Data1List = [D1 || {D1,_,_} <-AllDataList], Data2List = [D2 || {_,D2,_} <-AllDataList],  Data3List = [D3 || {_,_,D3} <-AllDataList],
      {lists:sum(Data1List)/length(Data1List),lists:sum(Data2List)/length(Data2List),lists:sum(Data3List)/length(Data3List)}
  end.