-module(wamp_util).

-export([json_encode/1]).
-export([json_decode/1]).

%% ===================================================================
%% Public
%% ===================================================================

json_encode(Term) ->
    jiffy:encode(Term).

json_decode(Raw) ->
    jiffy:decode(Raw).

%% ===================================================================
%% Private
%% ===================================================================
