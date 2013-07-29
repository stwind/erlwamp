-module(client).

-export([start_link/2]).
-export([call/3]).
-export([subscribe/2]).
-export([unsubscribe/2]).
-export([publish/3]).
-export([publish/4]).
-export([publish/5]).
-export([stop/1]).

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-include("wamp_msg.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(state, { parent }).

%% ===================================================================
%% Public
%% ===================================================================

start_link(Url, Parent) ->
    websocket_client:start_link(Url, ?MODULE, [Parent]).

call(Pid, Uri, Args) ->
    Msg = wamp_msg:call(Uri, Args),
    [_, CallId | _] = jiffy:decode(Msg),
    websocket_client:cast(Pid, {text, Msg}),
    receive
        {call_res, CallId, Res} ->
            Res
    end.

subscribe(Pid, Topic) ->
    websocket_client:cast(Pid, {text, wamp_msg:subscribe(Topic)}).

unsubscribe(Pid, Topic) ->
    websocket_client:cast(Pid, {text, wamp_msg:unsubscribe(Topic)}).

publish(Pid, Topic, Event) ->
    websocket_client:cast(Pid, {text, wamp_msg:publish(Topic, Event)}).

publish(Pid, Topic, Event, Exclude) ->
    websocket_client:cast(Pid, {text, wamp_msg:publish(Topic, Event, Exclude)}).

publish(Pid, Topic, Event, Exclude, Eligible) ->
    Msg = wamp_msg:publish(Topic, Event, Exclude, Eligible),
    websocket_client:cast(Pid, {text, Msg}).

stop(Pid) ->
    Pid ! stop.

%% ===================================================================
%% Callbacks
%% ===================================================================

init([Parent], _WSReq) ->
    {ok, #state{ parent = Parent}}.

websocket_handle({text, Msg}, _, State) ->
    {ok, Msg1} = wamp_msg:decode(Msg),
    handle_msg(Msg1, State),
    {ok, State}.

websocket_info(stop, _, State) ->
    {close, <<>>, State};

websocket_info(_, _, State) ->
    {ok, State}.

websocket_terminate(_Close, _, _State) ->
    ok.

%% ===================================================================
%% Callbacks
%% ===================================================================

handle_msg(#wamp_call_res{id = Id, result = Res}, #state{parent = Parent}) ->
    Parent ! {call_res, Id, Res};
handle_msg(#wamp_event{topic = Topic, event = Event}, #state{parent = Parent}) ->
    Parent ! {event, self(), Topic, Event};
handle_msg(_, _) ->
    ok.
