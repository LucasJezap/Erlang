-module(pollutionserver).
-author("Lukasz Jezapkowicz").

-export([start/0, stop/0, startServer/0,getMonitor/0,reset/0,addValue/4,addStation/2,removeValue/3,
  getDailyMean/2,getStationMean/2,getOneValue/3,getDailyInfoStation/2,getInfoStation/1,getParameterAmplitude/2]).

start() ->
  register(pollutionserver,spawn(?MODULE,startServer,[])).

stop() ->
  pollutionserver ! endServer.

startServer() ->
  server(pollution:createMonitor()).

% server
server(Monitor) ->
  receive
    endServer -> ok;
    {call, reset, {}, Pid} ->
      NewMonitor = Pid ! pollution:createMonitor(),
      server(NewMonitor);

    {call, getMonitor, {}, Pid} ->
      Pid ! Monitor,
      server(Monitor);

    {call, addStation, {StationName,LatAndLon}, Pid} ->
      NewMonitor = Pid ! pollution:addStation(StationName,LatAndLon,Monitor),
      server(NewMonitor);

    {call, addValue, {Info,Date,Type,Value}, Pid} ->
      NewMonitor = Pid ! pollution:addValue(Info,Date,Type,Value,Monitor),
      server(NewMonitor);

    {call, removeValue, {Info,Date,Type}, Pid} ->
      NewMonitor = Pid ! pollution:removeValue(Info,Date,Type,Monitor),
      server(NewMonitor);

    {call, getOneValue, {Info,Date,Type}, Pid} ->
      Pid ! pollution:getOneValue(Info,Date,Type,Monitor),
      server(Monitor);

    {call, getStationMean, {Info,Type}, Pid} ->
      Pid ! pollution:getStationMean(Info,Type,Monitor),
      server(Monitor);

    {call, getDailyMean, {Date,Type}, Pid} ->
      Pid ! pollution:getDailyMean(Date,Type,Monitor),
      server(Monitor);

    {call, getParameterAmplitude, {Info,Type}, Pid} ->
      Pid ! pollution:getParameterAmplitude(Info,Type,Monitor),
      server(Monitor);

    {call, getInfoStation, {Info}, Pid} ->
      Pid ! pollution:getInfoStation(Info,Monitor),
      server(Monitor);

    {call, getDailyInfoStation, {Info,Date}, Pid} ->
      Pid ! pollution:getDailyInfoStation(Info,Date,Monitor),
      server(Monitor)
  end.

command(Command,Parameters) ->
  pollutionserver ! {call, Command, Parameters, self()},
  receive
    Result -> Result
  end.

% extra functionality
reset() ->
  command(reset,{}).

getMonitor() ->
  command(getMonitor,{}).

% Function adding new station
addStation(StationName, LatAndLon) ->
  command(addStation, {StationName,LatAndLon}).

% Function adding measurement to given station (if it's in Monitor), if there
% is more than one matching station then i'm not adding anywhere (how should i know where)?
addValue(Info,Date,Type,Value) ->
  command(addValue, {Info,Date,Type,Value}).

% Function deleting measurement from given station (if it's in Monitor), if there
% is more than one matching station then i'm not deleting anywhere (how should i know where)?
removeValue(Info,Date,Type) ->
  command(removeValue, {Info,Date,Type}).

% Function returns measurement value from given station, type and date (or error message)
getOneValue(Info,Date,Type) ->
  command(getOneValue, {Info,Date,Type}).

% Function returns mean value of all measurements of given type from given station (or error message)
getStationMean(Info,Type) ->
  command(getStationMean, {Info,Type}).

% Function returns mean value of all measurements of given type in given date of all stations
getDailyMean(Date,Type) ->
  command(getDailyMean, {Date,Type}).

% Function returns difference between maximum and minimum measurement of given type on given station
getParameterAmplitude(Info,Type) ->
  command(getParameterAmplitude, {Info,Type}).

% Function returns all information about all measurements on given station
getInfoStation(Info) ->
  command(getInfoStation, {Info}).

% Function returns all information about all measurements on given station on given day
getDailyInfoStation(Info,Date) ->
  command(getDailyInfoStation, {Info,Date}).
