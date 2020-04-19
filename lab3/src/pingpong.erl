-module(pingpong).
-author("Lukasz Jezapkowicz").

%% API
-export([start/0,stop/0,play/1,ping/1,pong/0]).

start() ->
  register(ping,spawn(?MODULE,ping,[0])),
  register(pong,spawn(?MODULE,pong,[])).

play(N) ->
  ping ! {start, N}.

stop() ->
  ping ! stop,
  pong ! stop.

ping(Bounces) ->
  receive
    {start, N} ->
      io:format("Match begins!~n"),
      ping ! {ping, N},
      ping(Bounces);
    {ping, N} ->
      if
        N > 0 ->
          io:format("Ping time!~n"),
          timer:sleep(500),
          pong ! {pong,N},
          ping(Bounces+1);
        true ->
          pong ! {pong,N},
          io:format("That was the last ping!~n"),
          io:format("WOW! That was long ~B bounces!~n",[Bounces])
      end;
    stop -> ok
  after
    20000 -> ok
  end.

pong() ->
  receive
    {pong, N} ->
      if
        N > 0 ->
          io:format("Pong time!~n"),
          timer:sleep(500),
          ping ! {ping,N-1},
          pong();
        true ->
          io:format("That was the last pong!~n")
      end;
    stop -> ok
  after
    20000 -> ok
  end.