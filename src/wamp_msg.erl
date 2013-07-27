-module(wamp_msg).

-export([decode/1]).
-export([welcome/1]).
-export([callresult/2]).
-export([callerror/2]).
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

decode(Raw) ->
    client_msg(wamp_util:json_decode(Raw)).

welcome(SessionId) ->
    {ok, Vsn} = application:get_key(wamp, vsn),
    encode([?WELCOME, SessionId, 1, iolist_to_binary(["erlwamp/",Vsn])]).

callresult(Id, Reply) ->
    encode([?CALLRESULT, Id, Reply]).

callerror(Id, {Uri, Desc}) ->
    encode([?CALLERRROR, Id, Uri, Desc]);
callerror(Id, {Uri, Desc, Detail}) ->
    encode([?CALLERRROR, Id, Uri, Desc, Detail]).

event(Uri, Event) ->
    encode([?EVENT, Uri, Event]).

%% ===================================================================
%% Private
%% ===================================================================

encode(Msg) ->
    wamp_util:json_encode(Msg).

client_msg([?RPEFIX, Prefix, Uri]) ->
    {ok, #wamp_prefix{prefix = Prefix, uri = Uri}};
client_msg([?CALL, CallId, Uri | Args]) ->
    {ok, #wamp_call{id = CallId, uri = Uri, args = Args}};
client_msg([?SUBSCRIBE, Topic]) ->
    {ok, #wamp_sub{topic = Topic}};
client_msg([?UNSUBSCRIBE, Topic]) ->
    {ok, #wamp_unsub{topic = Topic}};
client_msg([?PUBLISH, Topic, Event]) ->
    client_msg([?PUBLISH, Topic, Event, true, []]);
client_msg([?PUBLISH, Topic, Event, ExcludeMe]) ->
    client_msg([?PUBLISH, Topic, Event, ExcludeMe, []]);
client_msg([?PUBLISH, Topic, Event, ExcludeMe, Eligible]) ->
    {ok, #wamp_publish{topic = Topic, event = Event, exclude_me = ExcludeMe, 
            eligible = Eligible}};
client_msg(_) ->
    {error, unknown_msg}.

%% ===================================================================
%% EUnit
%% ===================================================================

-ifdef(TEST).  
-include_lib("eunit/include/eunit.hrl").

welcome_test() ->
    application:load(wamp),
    ?assertMatch(<<"[0,\"session\",1,\"erlwamp/",_/binary>>,
        welcome(<<"session">>)).

callresult_test() ->
    Output = <<"[3,\"id\",{\"key\":\"value\"}]">>,
    ?assertMatch(Output, callresult(<<"id">>, {[{key, <<"value">>}]})).

-endif.
