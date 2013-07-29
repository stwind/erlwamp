-module(wamp_msg).

-export([decode/1]).
-export([welcome/1]).
-export([prefix/2]).
-export([call/2]).
-export([callresult/2]).
-export([callerror/2]).
-export([subscribe/1]).
-export([unsubscribe/1]).
-export([publish/2]).
-export([publish/3]).
-export([publish/4]).
-export([event/2]).

-include("wamp_msg.hrl").

-define(WELCOME, 0).
-define(PREFIX, 1).
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

prefix(Prefix, Uri) ->
    encode([?PREFIX, Prefix, Uri]).

call(Uri, Args) ->
    CallId = wamp_util:uid62(),
    encode([?CALL, CallId, Uri | Args]).

callresult(Id, Reply) ->
    encode([?CALLRESULT, Id, Reply]).

callerror(Id, {Uri, Desc}) ->
    encode([?CALLERRROR, Id, Uri, Desc]);
callerror(Id, {Uri, Desc, Detail}) ->
    encode([?CALLERRROR, Id, Uri, Desc, Detail]).

subscribe(Topic) ->
    encode([?SUBSCRIBE, Topic]).

unsubscribe(Topic) ->
    encode([?UNSUBSCRIBE, Topic]).

publish(Topic, Event) ->
    publish(Topic, Event, true, []).

publish(Topic, Event, ExcludeMe) ->
    publish(Topic, Event, ExcludeMe, []).

publish(Topic, Event, ExcludeMe, Eligible) ->
    encode([?PUBLISH, Topic, Event, ExcludeMe, Eligible]).

event(Uri, Event) ->
    encode([?EVENT, Uri, Event]).

%% ===================================================================
%% Private
%% ===================================================================

encode(Msg) ->
    wamp_util:json_encode(Msg).

client_msg([?WELCOME, SessionId, ProtoVer, Server]) ->
    {ok, #wamp_welcome{session_id = SessionId, proto_ver = ProtoVer, server = Server}};
client_msg([?PREFIX, Prefix, Uri]) ->
    {ok, #wamp_prefix{prefix = Prefix, uri = Uri}};
client_msg([?CALL, CallId, Uri | Args]) ->
    {ok, #wamp_call{id = CallId, uri = Uri, args = Args}};
client_msg([?CALLRESULT, CallId, Result]) ->
    {ok, #wamp_call_res{id = CallId, result = Result}};
client_msg([?CALLERRROR, CallId, Uri, Desc]) ->
    {ok, #wamp_call_err{id = CallId, uri = Uri, desc = Desc, detail = <<>>}};
client_msg([?CALLERRROR, CallId, Uri, Desc, Detail]) ->
    {ok, #wamp_call_err{id = CallId, uri = Uri, desc = Desc, detail = Detail}};
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
client_msg([?EVENT, Topic, Event]) ->
    {ok, #wamp_event{topic = Topic, event = Event}};
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
