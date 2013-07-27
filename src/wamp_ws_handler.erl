-module(wamp_ws_handler).

-export([init_state/2]).

-export([sockjs_init/2]).
-export([sockjs_handle/3]).
-export([sockjs_terminate/2]).

-include("wamp_msg.hrl").

-record(state, {
        handler :: module(),
        handler_state :: term(),
        id :: binary()
    }).

%% ===================================================================
%% Public
%% ===================================================================

init_state(Handler, HState) ->
    #state{handler = Handler, handler_state = Handler:init(HState)}.

%% ===================================================================
%% Callbacks
%% ===================================================================

sockjs_init(Conn, State) ->
    SessionId = session_id(),
    Msg = wamp_msg:welcome(SessionId),
    sockjs_session:send(Msg, Conn),
    {ok, State1} = call_handler(welcome, Conn, undefined, State#state{id = SessionId}),
    {ok, State1}.

sockjs_handle(Conn, Data, State) ->
    case wamp_msg:decode(Data) of
        {ok, Msg} ->
            handle_msg(Conn, Msg, State);
        {error, _} ->
            {ok, State}
    end.

sockjs_terminate(_Conn, _State) ->
    ok.

%% ===================================================================
%% Private
%% ===================================================================

handle_msg(Conn, #wamp_prefix{prefix = Prefix, uri = Uri}, State) ->
    {ok, State1} = call_handler(prefix, Conn, {Prefix, Uri}, State),
    {ok, State1};

handle_msg(Conn, #wamp_call{id = Id, uri = Uri, args = Args}, State) ->
    {Result, State1} = call_handler(call, Conn, {Uri, Args}, State),
    Msg = case Result of
        {ok, Reply} -> wamp_msg:callresult(Id, Reply);
        {error, Error} -> wamp_msg:callerror(Id, Error)
    end,
    sockjs_session:send(Msg, Conn),
    {ok, State1};

handle_msg(Conn, #wamp_sub{topic = Topic}, State) -> 
    {ok, State1} = call_handler(subscribe, Conn, Topic, State),
    {ok, State1};

handle_msg(Conn, #wamp_unsub{topic = Topic}, State) -> 
    {ok, State1} = call_handler(unsubscribe, Conn, Topic, State),
    {ok, State1};

handle_msg(Conn, #wamp_publish{
        topic = Topic, event = Event, 
        exclude_me = ExcludeMe, eligible = Eligible
    }, State) -> 
    Arg = {Topic, Event, ExcludeMe, Eligible},
    {ok, State1} = call_handler(publish, Conn, Arg, State),
    {ok, State1};

handle_msg(_, _, _) ->
    ok.

session_id() ->
    [[$0,$.|Id]] = io_lib:format("~.16f", [random:uniform()]),
    list_to_binary(Id).

call_handler(Fun, Conn, Args, #state{
        handler = Handler, handler_state = HState, id = SessionId} = State) ->
    {Reply, HState1} = do_call(Handler, Fun, {SessionId, Conn}, Args, HState),
    {Reply, State#state{handler_state = HState1}}.

do_call(Mod, Fun, Client, undefined, State) ->
    apply(Mod, Fun, [Client, State]);
do_call(Mod, Fun, Client, Args, State) ->
    apply(Mod, Fun, [Client, Args, State]).
