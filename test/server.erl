-module(server).

-export([init/1]).
-export([welcome/2]).
-export([handle_wamp_prefix/3]).
-export([handle_wamp_call/3]).
-export([handle_wamp_sub/3]).
-export([handle_wamp_unsub/3]).
-export([handle_wamp_pub/3]).
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

welcome(_Client, State) ->
    {ok, State}.

handle_wamp_prefix({_Prefix, _Uri}, _Client, State) ->
    {ok, State}.

handle_wamp_call({echo, _Env, [Msg]}, _Client, State) ->
    {{ok, Msg}, State};

handle_wamp_call(D, _, State) ->
    ?debugFmt("d ~p",[D]),
    {{ok, <<>>}, State}.

handle_wamp_sub({default, Topic}, Client, State) ->
    {ok, add_sub(Topic, Client, State)}.

handle_wamp_unsub({default, Topic}, Client, State) ->
    {ok, remove_sub(Topic, Client, State)}.

handle_wamp_pub({default, Topic, Event, Opts}, Client, State) ->
    Subs = get_subs(Topic),
    Subs1 = maybe_exclude_me(proplists:get_value(exclude, Opts), Client, Subs),
    Subs2 = maybe_eligible(proplists:get_value(eligible, Opts), Subs1),
    sent_event_to(Topic, Event, Subs2),
    {ok, State}.

terminate(_, State) ->
    {ok, State}.

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
