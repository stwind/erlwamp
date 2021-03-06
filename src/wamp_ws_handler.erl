-module(wamp_ws_handler).

-export([init_state/3]).

-export([sockjs_init/2]).
-export([sockjs_handle/3]).
-export([sockjs_terminate/2]).

-include("wamp_msg.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(state, {
        handler :: module(),
        dispatch :: term(),
        handler_state :: term(),
        id :: binary()
    }).

%% ===================================================================
%% Public
%% ===================================================================

init_state(Handler, Opts, Dispatch) ->
    #state{handler = Handler, dispatch = wamp_uri:compile(Dispatch), 
        handler_state = Handler:setup(Opts)}.

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

sockjs_terminate(Conn, State) ->
    call_handler(terminate, Conn, undefined, State),
    ok.

%% ===================================================================
%% Private
%% ===================================================================

handle_msg(Conn, #wamp_prefix{prefix = Prefix, uri = Uri}, State) ->
    {ok, State1} = call_handler(handle_wamp_prefix, Conn, {Prefix, Uri}, State),
    {ok, State1};

handle_msg(Conn, #wamp_call{id = Id, uri = Uri, args = Args}, State) ->
    {Name, Env} = match_uri(Uri, State),
    {Result, State1} = call_handler(handle_wamp_call, Conn, {Name, Env, Args}, State),
    Msg = case Result of
        {ok, Reply} -> wamp_msg:callresult(Id, Reply);
        {error, Error} -> wamp_msg:callerror(Id, Error)
    end,
    sockjs_session:send(Msg, Conn),
    {ok, State1};

handle_msg(Conn, #wamp_sub{topic = Topic}, State) -> 
    {ok, State1} = call_handler(handle_wamp_sub, Conn, match_uri(Topic, State), State),
    {ok, State1};

handle_msg(Conn, #wamp_unsub{topic = Topic}, State) -> 
    {ok, State1} = call_handler(handle_wamp_unsub, Conn, match_uri(Topic, State), State),
    {ok, State1};

handle_msg(Conn, #wamp_publish{
        topic = Topic, event = Event, 
        exclude_me = ExcludeMe, eligible = Eligible
    }, State) -> 
    {Name, Env} = match_uri(Topic, State),
    Arg = {Name, Env, Event, [{exclude, ExcludeMe},{eligible, Eligible}]},
    {ok, State1} = call_handler(handle_wamp_pub, Conn, Arg, State),
    {ok, State1};

handle_msg(_, _, _) ->
    ok.

session_id() ->
    random:seed(os:timestamp()),
    [[$0,$.|Id]] = io_lib:format("~.16f", [random:uniform()]),
    list_to_binary(Id).

call_handler(Fun, Conn, Arg, #state{
        handler = Handler, handler_state = HState} = State) ->
    {Reply, HState1} = do_call(Handler, Fun, Arg, client(Conn, State), HState),
    {Reply, State#state{handler_state = HState1}}.

do_call(Mod, Fun, undefined, Client, State) ->
    apply(Mod, Fun, [Client, State]);
do_call(Mod, Fun, Arg, Client, State) ->
    apply(Mod, Fun, [Arg, Client, State]).

client(Conn, #state{id = SessionId}) ->
    {SessionId, Conn}.

match_uri(Uri, #state{dispatch = Dispatch}) ->
    wamp_uri:match(Uri, Dispatch).
