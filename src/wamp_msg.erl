-module(wamp_msg).

-export([welcome/1]).
-export([callresult/2]).
-export([callerror/4]).
-export([event/2]).

-include("wamp_msg.hrl").

-define(WELCOME, 0).
-define(RPEFIX, 1).
-define(CALL, 2).
-define(CALLRESULT, 3).
-define(CALLERRROR, 4).
-define(SUBSCRIBE, 5).
-define(UNSUBSCRIBE, 6).
-define(PUBLISH, 7).
-define(EVENT, 8).

%% ===================================================================
%% Public
%% ===================================================================

welcome(SessionId) ->
    {ok, Vsn} = application:get_key(wamp, vsn),
    encode([?WELCOME, SessionId, 1, iolist_to_binary(["wamp-erlang/",Vsn])]).

callresult(Id, Reply) ->
    encode([?CALLRESULT, Id, Reply]).

callerror(Id, Uri, Desc, Detail) ->
    encode([?CALLERRROR, Id, Uri, Desc, Detail]).

event(Uri, Event) ->
    encode([?EVENT, Uri, Event]).

%% ===================================================================
%% Private
%% ===================================================================

encode(Msg) ->
    jiffy:encode(Msg).
