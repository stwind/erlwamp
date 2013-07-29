-module(wamp).

-export([start/0]).
-export([stop/0]).

-export([init_sockjs_state/3]).
-export([info/1]).
-export([notify/3]).

%% ===================================================================
%% Public
%% ===================================================================

start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

init_sockjs_state(Url, Handler, HandlerState) ->
    State = wamp_ws_handler:init_state(Handler, HandlerState),
    sockjs_handler:init_state(Url, wamp_ws_handler, State, []).

info(Conn) ->
    sockjs_session:info(Conn).

notify(Topic, Event, Conn) ->
    Msg = wamp_msg:event(Topic, Event),
    sockjs_session:send(Msg, Conn).
