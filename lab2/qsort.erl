%%%-------------------------------------------------------------------
%%% @author Lukasz Jezapkowicz
%%% Created : 14. mar 2020 09:54
%%%-------------------------------------------------------------------
-module(qsort).

%% API
-export([qs/1,randomElems/3,compareSpeeds/3]).

lessThan(L,A) -> [X || X <- L, X < A].

grtEqThan(L,A) -> [X || X <- L, X >= A].

qs([]) -> [];
qs([Pivot|Tail]) -> qs(lessThan(Tail,Pivot)) ++ [Pivot] ++ qs(grtEqThan(Tail,Pivot)).

randomElems(N,Min,Max)-> [rand:uniform(Max-Min+1) + Min - 1 || _ <- lists:seq(1, N)].

compareSpeeds(List, Fun1, Fun2) ->
  T1 = timer:tc(qsort,Fun1,[List]),
  T2 = timer:tc(lists,Fun2,[List]),
  io:format("The speed of ~p is ~ps while speed of ~p is ~ps~n",
    [Fun1,element(1,T1)/math:pow(10,6),Fun2,element(1,T2)/math:pow(10,6)]).

%% przykładowo
%% B = qsort:randomElems(200000,5,5000).
%% qsort:compareSpeeds(B,qs,sort).
%% The speed of qs is 0.265s while speed of sort is 0.079s