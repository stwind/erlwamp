-module(server).

-export([init/1]).
-export([welcome/2]).
-export([prefix/3]).
-export([call/3]).
-export([subscribe/3]).
-export([unsubscribe/3]).
-export([publish/3]).
-export([terminate/2]).

-record(state, { }).

-include_lib("eunit/include/eunit.hrl").

-define(TOPICS, topics).
-define(CONNS, conns).

%% ===================================================================
%% Callbacks
%% ===================================================================

init([]) ->
    ets:new(?TOPICS,[named_table,public]),
    ets:new(?CONNS,[named_table,public]),
    #state{}.

welcome(_, State) ->
    {ok, State}.

prefix(_, {_Prefix, _Uri}, State) ->
    {ok, State}.

call(_, {<<"echo">>, [Msg]}, State) ->
    {{ok, Msg}, State};

call(_, _, State) ->
    {{ok, <<>>}, State}.

subscribe(Client, Topic, State) ->
    {ok, add_sub(Topic, Client, State)}.

unsubscribe(Client, Topic, State) ->
    {ok, remove_sub(Topic, Client, State)}.

publish(Client, {Topic, Event, Exclude, Eligible}, State) ->
    Subs = get_subs(Topic),
    Subs1 = maybe_exclude_me(Exclude, Client, Subs),
    Subs2 = maybe_eligible(Eligible, Subs1),
    sent_event_to(Topic, Event, Subs2),
    {ok, State}.

terminate(_, _) ->
    ok.

%% ===================================================================
%% Private
%% ===================================================================

add_sub(Topic, {Sid, Conn}, State) ->
    Subs = get_subs(Topic),
    ets:insert(?TOPICS, {Topic, lists:usort([Conn | Subs])}),
    ets:insert(?CONNS, {Sid, Conn}),
    State.

remove_sub(Topic, {Sid, Conn}, State) ->
    Subs = get_subs(Topic),
    ets:insert(?TOPICS, {Topic, lists:delete(Conn, Subs)}),
    ets:delete(?CONNS, Sid),
    State.

get_subs(Topic) ->
    case ets:lookup(?TOPICS, Topic) of
        [{Topic, Subs}] ->
            Subs;
        [] ->
            ets:insert(?TOPICS, {Topic, []}),
            []
    end.

maybe_exclude_me(true, {_, Conn}, Subs) ->
    lists:delete(Conn, Subs);
maybe_exclude_me(false, _, Subs) ->
    Subs.

maybe_eligible([], Subs) ->
    Subs;
maybe_eligible([Sid | Rest], Subs) ->
    case ets:lookup(?CONNS, Sid) of
        [{Sid, Conn}] ->
            maybe_eligible(Rest, lists:delete(Conn, Subs));
        [] ->
            maybe_eligible(Rest, Subs)
    end.

sent_event_to(Topic, Event, Subs) ->
    [wamp:notify(C, Topic, Event) || C <- Subs].
