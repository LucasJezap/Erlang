%%%-------------------------------------------------------------------
%%% @author Lukasz Jezapkowicz
%%% Created : 24. mar 2020 09:54
%%%-------------------------------------------------------------------
-module(pollution).
-author("Lukasz Jezapkowicz").

%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3
        ,getParameterAmplitude/3,getInfoStation/2,getDailyInfoStation/3]).

%% General idea - list of records of type {Name,LatAndLon,set()}

% Creates new Monitor
createMonitor() -> [].

% Function checks if given station name or latitude and longitude aren't in list yet
search(StationName,LatAndLon,Monitor) ->
  lists:any(fun({S,_,_}) -> S == StationName end, Monitor) or
  lists:any(fun({_,L,_}) -> L == LatAndLon end, Monitor).

% Function adding new station
addStation(StationName,LatAndLon,Monitor) ->
  case search(StationName,LatAndLon,Monitor) of
      false -> [ {StationName,LatAndLon,sets:new()} | Monitor];
      _ -> Monitor
  end.

% Function checks if there is station with given Info and returns it if there is
searchValue(Info,Monitor) ->
  [{Name,L,S} || {Name,L,S} <- Monitor, Name == Info]  ++
  [{Name,L,S} || {Name,L,S} <- Monitor, L == Info].

% Function adding measurement to given station (if it's in Monitor), if there
% is more than one matching station then i'm not adding anywhere (how should i know where)?
addValue(Info,Date,Type,Value,Monitor) ->
  case searchValue(Info,Monitor) of
    [H] ->
      {StationName,LatAndLon,Set} = H,
      case sets:is_element({Type,Value,Date},Set) of
        false ->
          L = lists:delete(H,Monitor),
          NewSet = sets:add_element({Type,Value,Date},Set),
          [{StationName,LatAndLon,NewSet} | L];
        true -> Monitor
      end;
    _ -> Monitor
  end.

% Function deleting measurement from given station (if it's in Monitor), if there
% is more than one matching station then i'm not deleting anywhere (how should i know where)?
removeValue(Info,Date,Type,Monitor) ->
  case searchValue(Info,Monitor) of
    [H] ->
      {StationName,LatAndLon,Set} = H,
      L = sets:to_list(Set),
      NewL = [{T,V,D} || {T,V,D} <- L, T /= Type, D /= Date] ++ [{T,V,D} || {T,V,D} <- L, T == Type, D /= Date],
      NewSet = sets:from_list(NewL),
      L2 = lists:delete(H,Monitor),
      [{StationName,LatAndLon,NewSet} | L2];
    _ -> Monitor
  end.

% Function returns measurement value from given station, type and date (or error message)
getOneValue(Info,Date,Type,Monitor) ->
  case searchValue(Info,Monitor) of
    [H] ->
      {_,_,Set} = H,
      L = sets:to_list(Set),
      NewL = [{T,V,D} || {T,V,D} <- L, T == Type, D == Date],
      case NewL of
        [] -> "There was no such measurement!";
        _ -> element(2,lists:nth(1,NewL))
      end;
    _ -> "There is no such station!"
  end.

% Function returns mean value of all measurements of given type from given station (or error message)
getStationMean(Info,Type,Monitor) ->
  case searchValue(Info,Monitor) of
    [H] ->
      {_,_,Set} = H,
      L = sets:to_list(Set),
      NewL = [{T,V,D} || {T,V,D} <- L, T == Type],
      case NewL of
        [] -> "There are no measurement of this type!";
        _ ->
          Sum = lists:foldl(fun({_,V,_},S) -> V + S end, 0, NewL),
          Length = lists:foldl(fun({_,_,_},S) -> 1 + S end, 0, NewL),
          Sum / Length
      end;
    _ -> "There is no such station!"
  end.

% Function returns sum of all measurements of given type in given date of all stations
getDailySum(_,_,[]) -> 0;
getDailySum(Date,Type,[H | TL]) ->
  {_,_,Set} = H,
  L = sets:to_list(Set),
  NewL = [{T,V,D} || {T,V,D} <- L, T == Type, element(1,D) == Date],
  Sum = lists:foldl(fun({_,V,_},S) -> V + S end,0,NewL),
  Sum + getDailySum(Date,Type,TL).

% Function returns count of all measurements of given type in given date of all stations
getDailyCount(_,_,[]) -> 0;
getDailyCount(Date,Type,[H | TL]) ->
  {_,_,Set} = H,
  L = sets:to_list(Set),
  NewL = [{T,V,D} || {T,V,D} <- L, T == Type, element(1,D) == Date],
  Sum = lists:foldl(fun({_,_,_},S) -> 1 + S end,0,NewL),
  Sum + getDailyCount(Date,Type,TL).

% Function returns mean value of all measurements of given type in given date of all stations
getDailyMean(Date,Type,Monitor) ->
  getDailySum(Date,Type,Monitor) / getDailyCount(Date,Type,Monitor).

% BONUS FUNCTIONS - I choose 2 functions which i find particularly useful

% Function returns difference between maximum and minimum measurement of given type on given station
getParameterAmplitude(Info,Type,Monitor) ->
  case searchValue(Info,Monitor) of
    [H] ->
      {_,_,Set} = H,
      L = sets:to_list(Set),
      NewL = [V || {T,V,_} <- L, T == Type],
      case NewL of
        [] -> "There are no measurement of this type!";
        _ ->
          Max_value = lists:max(NewL),
          Min_value = lists:min(NewL),
          [D1] = [D || {_,V,D} <- L, V == Max_value],
          [D2] = [D || {_,V,D} <- L, V == Min_value],
          {{"Amplitude",Max_value-Min_value},{"Max Value",Max_value,D1},{"Min Value",Min_value,D2}}
      end;
    _ -> "There is no such station!"
  end.

% Helping recursive function to print data
printInfo([]) -> {ok};
printInfo([H | T]) ->
  io:format("~p~n",[H]),
  printInfo(T).


% Function returns all information about all measurements on given station
getInfoStation(Info,Monitor) ->
  case searchValue(Info,Monitor) of
    [H] ->
      {StationName,_,Set} = H,
      L = sets:to_list(Set),
      case L of
        [] -> "There are no measurements!";
        _ ->
          io:format("Now printing all measurements on station ~s~n",[StationName]),
          printInfo(L)
      end;
    _ -> "There is no such station!"
  end.

% Function returns all information about all measurements on given station on given day
getDailyInfoStation(Info,Date,Monitor) ->
  case searchValue(Info,Monitor) of
    [H] ->
      {StationName,_,Set} = H,
      L = [{T,V,D} || {T,V,D} <- sets:to_list(Set), element(1,D) == Date],
      case L of
        [] -> "There are no measurements!";
        _ ->
          io:format("Now printing all measurements on station ~s on date ~w~n",[StationName,Date]),
          printInfo(L)
      end;
    _ -> "There is no such station!"
  end.