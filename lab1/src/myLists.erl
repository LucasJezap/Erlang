%%%-------------------------------------------------------------------
%%% @author yyy
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. mar 2020 10:25
%%%-------------------------------------------------------------------
-module(myLists).
-author("yyy").

%% API
-export([contains/2,duplicateElements/1,sumFloats/1,sumFloats2/2]).

contains(V,[H|T]) -> (H == V) or contains(V,T);
contains(_,[]) -> false.

duplicateElements([H|T]) -> [H,H | duplicateElements(T)];
duplicateElements([]) -> [].

sumFloats([H|T]) when is_float(H) -> H + sumFloats(T);
sumFloats([_|T]) -> sumFloats(T);
sumFloats([]) -> 0.

sumFloats2([H|T],Sum) when is_float(H) -> sumFloats2(T,Sum+H);
sumFloats2([_|T],Sum) -> sumFloats2(T,Sum);
sumFloats2([],Sum) -> Sum.