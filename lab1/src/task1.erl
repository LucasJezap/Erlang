%%%-------------------------------------------------------------------
%%% @author yyy
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. mar 2020 09:41
%%%-------------------------------------------------------------------
-module(task1).
-author("yyy").

%% API
-export([power/2]).

power(A,B) when B>0 -> A * power(A,B-1);
power(_,0) -> 1.
