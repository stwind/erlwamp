-record(wamp_welcome, { session_id, proto_ver, server }).

-record(wamp_prefix, { prefix, uri }).

-record(wamp_call, { id, uri, args }).

-record(wamp_call_res, { id, result }).

-record(wamp_call_err, { id, uri, desc, detail }).

-record(wamp_sub, { topic }).

-record(wamp_unsub, { topic }).

-record(wamp_publish, { topic, event, exclude_me, eligible }).

-record(wamp_event, { topic, event }).
