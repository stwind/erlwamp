-module(wamp_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(PORT, 8081).
-define(URL, "ws://localhost:8081/wamp/websocket").
-define(wait(Ms), timer:sleep(Ms)).

%% ===================================================================
%% Public
%% ===================================================================

wamp_test_() ->
    {setup, fun setup/0, fun cleanup/1,[
            {"call", fun call/0},
            {"pubsub", fun pubsub/0}
        ]}.

setup() ->
    ok = application:start(xmerl),
    ok = application:start(sockjs),
    ok = application:start(wamp),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    Server = wamp:init_sockjs_state(<<"/wamp">>, server, []),
    VRoutes = [{<<"/wamp/[...]">>, sockjs_cowboy_handler, Server}],
    Dispatch = cowboy_router:compile([{'_',  VRoutes}]),
    cowboy:start_http(wamp_http_listener, 10, [{port, ?PORT}],
        [{env, [{dispatch, Dispatch}]}]).

cleanup(_) ->
    ok = application:stop(xmerl),
    ok = application:stop(sockjs),
    ok = application:stop(ranch),
    ok = application:stop(wamp),
    ok = application:stop(crypto),    
    ok = application:stop(cowboy).

%% ===================================================================
%% Cases
%% ===================================================================

call() ->
    {ok, C} = start_client(),
    ?assertMatch(<<"hello">>, client:call(C, <<"echo">>, [<<"hello">>])).

pubsub() ->
    {ok, C1} = start_client(),
    {ok, C2} = start_client(),
    Topic = <<"topic1">>,
    ok = client:subscribe(C1, Topic), ?wait(100),
    ok = client:subscribe(C2, Topic), ?wait(100),
    client:publish(C1, Topic, <<"from c1">>), ?wait(100),
    ?assertMatch({Topic, <<"from c1">>}, ensure_event_on(C2)),
    ?assertMatch(none, ensure_event_on(C1)).

%% ===================================================================
%% Private
%% ===================================================================

start_client() ->
    client:start_link(?URL, self()).

ensure_event_on(Pid) ->
    receive
        {event, Pid, Topic, Event} ->
            {Topic, Event}
    after 0 ->
        none
    end.
