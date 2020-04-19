-module(pollution_gen_server).
-behaviour(gen_server).
-versin(1.0).
-author("Lukasz Jezapkowicz").

-export([start/0, stop/0, startServer/1,init/1,handle_call/3, handle_cast/2, terminate/2, getMonitor/0,reset/0,addValue/4,addStation/2,removeValue/3,
  getDailyMean/2,getStationMean/2,getOneValue/3,getDailyInfoStation/2,getInfoStation/1,getParameterAmplitude/2]).

stop() ->
  gen_server:cast(?MODULE, {stop}).

startServer(State) ->
  gen_server:start_link({local, ?MODULE},?MODULE,State,[]).
  
start() ->
  startServer(pollution:createMonitor()).

init(State) ->
	{ok, State}.
	
% extra functionality
reset() ->
  gen_server:call(?MODULE, {reset,{}}).

getMonitor() ->
  gen_server:call(?MODULE, {getMonitor,{}}).

% Function adding new station
addStation(StationName, LatAndLon) ->
  gen_server:call(?MODULE, {addStation, {StationName,LatAndLon}}).

% Function adding measurement to given station (if it's in Monitor), if there
% is more than one matching station then i'm not adding anywhere (how should i know where)?
addValue(Info,Date,Type,Value) ->
  gen_server:call(?MODULE, {addValue, {Info,Date,Type,Value}}).

% Function deleting measurement from given station (if it's in Monitor), if there
% is more than one matching station then i'm not deleting anywhere (how should i know where)?
removeValue(Info,Date,Type) ->
  gen_server:call(?MODULE, {removeValue, {Info,Date,Type}}).

% Function returns measurement value from given station, type and date (or error message)
getOneValue(Info,Date,Type) ->
  gen_server:call(?MODULE, {getOneValue, {Info,Date,Type}}).

% Function returns mean value of all measurements of given type from given station (or error message)
getStationMean(Info,Type) ->
  gen_server:call(?MODULE, {getStationMean, {Info,Type}}).

% Function returns mean value of all measurements of given type in given date of all stations
getDailyMean(Date,Type) ->
  gen_server:call(?MODULE, {getDailyMean, {Date,Type}}).

% Function returns difference between maximum and minimum measurement of given type on given station
getParameterAmplitude(Info,Type) ->
  gen_server:call(?MODULE, {getParameterAmplitude, {Info,Type}}).

% Function returns all information about all measurements on given station
getInfoStation(Info) ->
  gen_server:call(?MODULE, {getInfoStation, {Info}}).

% Function returns all information about all measurements on given station on given day
getDailyInfoStation(Info,Date) ->
  gen_server:call(?MODULE, {getDailyInfoStation, {Info,Date}}).
  

handle_call({reset,{}},_FROM,_) ->
	{reply, ok, pollution:createMonitor()};
	
handle_call({getMonitor,{}},_FROM,M) ->
	{reply, M, M};
	
handle_call({addStation, {StationName,LatAndLon}},_FROM,M) ->
	{reply, ok, pollution:addStation(StationName,LatAndLon,M)};
	
handle_call({addValue, {Info,Date,Type,Value}},_FROM,M) ->
	{reply, ok, pollution:addValue(Info,Date,Type,Value,M)};

handle_call({removeValue, {Info,Date,Type}},_FROM,M) ->
	{reply, ok, pollution:removeValue(Info,Date,Type,M)};
	
handle_call({getOneValue, {Info,Date,Type}},_FROM,M) ->
	{reply, pollution:getOneValue(Info,Date,Type,M),M};
	
handle_call({getStationMean, {Info,Type}},_FROM,M) ->
	{reply, pollution:getStationMean(Info,Type,M),M};
	
handle_call({getDailyMean, {Date,Type}},_FROM,M) ->
	{reply, pollution:getDailyMean(Date,Type,M),M};
	
handle_call({getParameterAmplitude, {Info,Type}},_FROM,M) ->
	{reply, pollution:getParameterAmplitude(Info,Type,M),M};
	
handle_call({getInfoStation, {Info}},_FROM,M) ->
	{reply, pollution:getInfoStation(Info,M),M};
	
handle_call({getDailyInfoStation, {Info,Date}},_FROM,M) ->
	{reply, pollution:getDailyInfoStation(Info,Date,M),M}.
	
handle_cast({stop},StopState) ->
	{stop, normal, StopState}.
	
terminate(_Reason, _State) ->
	ok.