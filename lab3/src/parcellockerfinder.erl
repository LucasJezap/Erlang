%%%-------------------------------------------------------------------
%%% @author yyy
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. kwi 2020 09:30
%%%-------------------------------------------------------------------
-module(parcellockerfinder).
-author("Lukasz Jezapkowicz").

%% API
-export([generateLockers/1,generatePeople/1,findMyParcelLocker/2,findAllSequential/2,
  findAllParallel/2,findParallel/3,findAllLessParallel/2,findGroups/3,compareSpeed/2]).

generatePeople(N) ->
  [{person,rand:uniform(10000),rand:uniform(10000)} || _ <- lists:seq(1,N)].

generateLockers(N) ->
  [{locker,rand:uniform(10000),rand:uniform(10000)} || _ <- lists:seq(1,N)].

getDistance(X1,Y1,X2,Y2) ->
  math:sqrt(math:pow(X2-X1,2)+math:pow(Y2-Y1,2)).

findMyParcelLocker(PersonLocation, LockerLocations) ->
  {_,X1,Y1} = PersonLocation,
  Distances = [{getDistance(X1,Y1,X2,Y2),Locker,X2,Y2} || {Locker,X2,Y2} <- LockerLocations],
  {_,RightLocker,RightX2,RightY2} = lists:min(Distances),
  {PersonLocation,{RightLocker,RightX2,RightY2}}.

% SEQUENTIAL
findAllSequential(PersonLocations, LockerLocations) ->
  [findMyParcelLocker(Person,LockerLocations) || Person <- PersonLocations].

% PARALLEL
findAllParallel(PersonLocations, LockerLocations) ->
  F = fun (Person) -> spawn(?MODULE,findParallel,[Person,LockerLocations,self()]) end,
  lists:foreach(F,PersonLocations),
  getResults([],length(PersonLocations)).

getResults(List, Length) ->
  receive
    Pair ->
      case Length of
        1 -> [Pair | List];
        _ -> getResults([Pair | List],Length-1)
      end
  end.

findParallel(Person,LockerLocations,Parent) ->
  Parent ! findMyParcelLocker(Person,LockerLocations).

% LESS PARALLEL

findAllLessParallel(PersonLocations, LockerLocations) ->
  ChunkLength = trunc(math:ceil(length(PersonLocations)/4)),
  People1 = lists:sublist(PersonLocations,ChunkLength),
  People2 = lists:sublist(PersonLocations,ChunkLength+1,ChunkLength),
  People3 = lists:sublist(PersonLocations,ChunkLength*2+1,ChunkLength),
  People4 = lists:sublist(PersonLocations,ChunkLength*3+1,ChunkLength),
  F = fun (Group) -> spawn(?MODULE,findGroups,[Group,LockerLocations,self()]) end,
  lists:foreach(F,[People1,People2,People3,People4]),
  getResults([],4).

findGroups(Group, LockerLocations, Parent) ->
  L = [findMyParcelLocker(Person,LockerLocations) || Person <- Group],
  Parent ! L.

compareSpeed(PersonLocations, LockerLocations) ->
  {T1,_} = timer:tc(?MODULE,findAllSequential,[PersonLocations,LockerLocations]),
  {T2,_} = timer:tc(?MODULE,findAllParallel,[PersonLocations,LockerLocations]),
  {T3,_} = timer:tc(?MODULE,findAllLessParallel,[PersonLocations,LockerLocations]),
  io:format("The time to find the answer sequentially is ~ps~n",[T1/math:pow(10,6)]),
  io:format("The time to find the answer parallelly is ~ps~n",[T2/math:pow(10,6)]),
  io:format("The time to find the answer less parallelly is ~ps~n",[T3/math:pow(10,6)]).
