-record(wamp_prefix, { prefix, uri }).

-record(wamp_call, { id, uri, args }).

-record(wamp_sub, { topic }).

-record(wamp_unsub, { topic }).

-record(wamp_publish, { topic, event, exclude_me, eligible }).
