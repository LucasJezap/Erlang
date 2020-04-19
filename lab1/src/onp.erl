%%%-------------------------------------------------------------------
%%% @author Lukasz Jezapkowicz
%%% Created : 03. mar 2020 10:44
%%%-------------------------------------------------------------------
-module(onp).

%% API
-export([onp/1]).

onp(Input) ->
  Elements = string:tokens(Input," "),
  Stack = [],
  onp(Elements,Stack).

onp([],[A]) -> A;

onp([A,B | T],[]) ->
  try
    onp(T,[list_to_float(B),list_to_float(A)])
  catch error:badarg ->
    onp1([A,B | T],[])
  end;

onp([A | T1],[X,Y | T2]) when A == "+" -> onp(T1,[Y+X | T2]);

onp([A | T1],[X,Y | T2]) when A == "*" -> onp(T1,[Y*X | T2]);

onp([A | T1],[X,Y | T2]) when A == "-" -> onp(T1,[Y-X | T2]);

onp([A | T1],[X,Y | T2]) when A == "/" ->
  case X of
    0 when Y > 0 -> "infinity";
    0 when Y < 0 -> "-infinity";
    _ -> onp(T1,[Y/X | T2])
  end;

onp([A | T1],[X | T2]) when A == "sqrt" -> onp(T1,[math:sqrt(X) | T2]);

onp([A | T1],[X,Y | T2]) when A == "pow" -> onp(T1,[math:pow(Y,X) | T2]);

onp([A | T1],[X | T2]) when A == "sin" -> onp(T1,[math:sin(X) | T2]);

onp([A | T1],[X | T2]) when A == "cos" -> onp(T1,[math:cos(X) | T2]);

onp([A | T1],[X | T2]) when A == "tan" -> onp(T1,[math:tan(X) | T2]);

onp([A | T1],[X | T2]) when A == "ctan" -> onp(T1,[1/math:tan(X) | T2]);

onp([A | T1],[X | T2]) when A == "jump" -> onp(T1,[math:pow(X,X) | T2]);

onp([A | T1],[X,Y | T2]) when A == "dance" -> onp(T1,[floor(Y) rem ceil(X) | T2]);


onp([A | T1],S) ->
  try
    onp(T1,[list_to_float(A) | S])
  catch error:badarg ->
    onp(T1,[list_to_integer(A) | S])
  end.



%% pomocnicze funkcje pozwalajace na wprowadzanie zarówno floatow jak i integerow jako 2 pierwszych elementow
onp1([A,B | T],[]) ->
  try
    onp(T,[list_to_float(B),list_to_integer(A)])
  catch error:badarg ->
    onp2([A,B | T],[])
  end.

onp2([A,B | T],[]) ->
  try
    onp(T,[list_to_integer(B),list_to_float(A)])
  catch error:badarg ->
    onp3([A,B | T],[])
  end.

onp3([A,B | T],[]) ->
  try
    onp(T,[list_to_integer(B),list_to_integer(A)])
  catch error:badarg ->
    onp4([A,B | T],[])
  end.

onp4([A,B | T],[]) ->
  try
    onp([B | T],[list_to_float(A)])
  catch error:badarg ->
    onp([B | T],[list_to_integer(A)])
  end.




