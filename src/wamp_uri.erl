-module(wamp_uri).

-export([compile/1]).
-export([match/2]).
-export([path_info/1]).
-export([binding/2]).
-export([binding/3]).
-export([qs_val/2]).
-export([qs_val/3]).

-include_lib("eunit/include/eunit.hrl").

-record(env, {
        bindings = [] :: list({atom(), binary()}),
        path_info = [] :: list(binary()),
        qs = [] :: list({atom(), binary()})
    }).

%% ===================================================================
%% Public
%% ===================================================================

compile(URIs) ->
    compile(URIs, []).

match(Path, Dispatch) ->
    {Path1, Query} = split_qs(Path),
    case match(Dispatch, Path1, []) of
        {ok, Name, PathBinds, PathInfo} ->
            {Name, #env{
                    bindings = PathBinds, 
                    path_info = PathInfo, 
                    qs = parse_qs(Query)}};
        {error, notfound} ->
            {default, Path}
    end.

path_info(#env{path_info = Info}) ->
    Info.

qs_val(Key, Env) ->
    qs_val(Key, Env, undefined).

qs_val(Key, #env{qs = Vals}, Default) ->
    case lists:keyfind(Key, 1, Vals) of
        {Key, Value} -> Value;
        _ -> Default
    end.

binding(Key, Env) ->
    binding(Key, Env, undefined).

binding(Key, #env{bindings = Bindings}, Default) ->
    case lists:keyfind(Key, 1, Bindings) of
        {Key, Value} -> Value;
        _ -> Default
    end.

%% ===================================================================
%% Private
%% ===================================================================

compile([], Acc) ->
    lists:reverse(Acc);
compile([{PathMatch, Name}|Tail], Acc) ->
    compile([{PathMatch, [], Name}|Tail], Acc);
compile([{PathMatch, Constraints, Name}|Tail], Acc) when is_list(PathMatch) ->
    compile([{iolist_to_binary(PathMatch), Constraints, Name}|Tail], Acc);
compile([{<< $/, PathMatch/binary >>, Constraints, Name}|Tail], Acc) ->
    PathRules = compile_rules(PathMatch, [], [], <<>>),
    Paths = [{lists:reverse(R), Constraints, Name} || R <- PathRules],
    compile(Tail, Paths ++ Acc).

compile_rules(<<>>, Segments, Rules, <<>>) ->
    [Segments|Rules];
compile_rules(<<>>, Segments, Rules, Acc) ->
    [[Acc|Segments]|Rules];
compile_rules(<< $/, Rest/binary >>, Segments, Rules, <<>>) ->
    compile_rules(Rest, Segments, Rules, <<>>);
compile_rules(<< $/, Rest/binary >>, Segments, Rules, Acc) ->
    compile_rules(Rest, [Acc|Segments], Rules, <<>>);
compile_rules(<< $:, Rest/binary >>, Segments, Rules, <<>>) ->
    {NameBin, Rest2} = compile_binding(Rest, <<>>),
    Name = binary_to_atom(NameBin, utf8),
    compile_rules(Rest2, Segments, Rules, Name);
compile_rules(<< $[, $., $., $., $], Rest/binary >>, Segments, Rules, <<>>) ->
    compile_rules(Rest, ['...'|Segments], Rules, <<>>);
compile_rules(<< $[, $., $., $., $], Rest/binary >>, Segments, Rules, Acc) ->
    compile_rules(Rest, ['...', Acc|Segments], Rules, Acc);
compile_rules(<< C, Rest/binary >>, Segments, Rules, Acc) ->
    compile_rules(Rest, Segments, Rules, << Acc/binary, C >>).

compile_binding(<<>>, <<>>) ->
    erlang:error(badarg);
compile_binding(Rest = <<>>, Acc) ->
    {Acc, Rest};
compile_binding(Rest = << C, _/binary >>, Acc)
when C =:= $/; C =:= $[; C =:= $] ->
    {Acc, Rest};
compile_binding(<< C, Rest/binary >>, Acc) ->
    compile_binding(Rest, << Acc/binary, C >>).

match([], _, _) ->
    {error, notfound};
match([{PathMatch, Name} | Rest], Tokens, Bindings) ->
    match([{PathMatch, [], Name} | Rest], Tokens, Bindings);
match([{PathMatch, Constraints, Name} | Rest], Tokens, 
    Bindings) when is_list(Tokens) ->
    case list_match(Tokens, PathMatch, Bindings) of
        false ->
            match(Rest, Tokens, Bindings);
        {true, PathBinds, PathInfo} ->
            case check_constraints(Constraints, PathBinds) of
                {ok, PathBinds2} ->
                    {ok, Name, PathBinds2, PathInfo};
                nomatch ->
                    match(Rest, Tokens, Bindings)
            end
    end;
match(Dispatch, Path, Bindings) ->
    match(Dispatch, split_path(Path), Bindings).

split_path(Path) ->
    tl(binary:split(Path, <<"/">>, [global])).

split_qs(Path) ->
    case binary:split(Path, <<"?">>, [global]) of
        [Path1] ->
            {Path1, <<>>};
        [Path1, Query] ->
            {Path1, Query}
    end.

check_constraints([], Bindings) ->
    {ok, Bindings};
check_constraints([Constraint|Tail], Bindings) ->
    Name = element(1, Constraint),
    case lists:keyfind(Name, 1, Bindings) of
        false ->
            check_constraints(Tail, Bindings);
        {_, Value} ->
            case check_constraint(Constraint, Value) of
                true ->
                    check_constraints(Tail, Bindings);
                {true, Value2} ->
                    Bindings2 = lists:keyreplace(Name, 1, Bindings,
                        {Name, Value2}),
                    check_constraints(Tail, Bindings2);
                false ->
                    nomatch
            end
    end.

check_constraint({_, int}, Value) ->
    try {true, list_to_integer(binary_to_list(Value))}
        catch _:_ -> false
    end;
check_constraint({_, function, Fun}, Value) ->
    Fun(Value).

list_match(List, ['...'], Binds) ->
    {true, Binds, List};
%% Atom '_' matches anything, continue.
list_match([_E|Tail], ['_'|TailMatch], Binds) ->
    list_match(Tail, TailMatch, Binds);
%% Both values match, continue.
list_match([E|Tail], [E|TailMatch], Binds) ->
    list_match(Tail, TailMatch, Binds);
%% Bind E to the variable name V and continue,
%% unless V was already defined and E isn't identical to the previous value.
list_match([E|Tail], [V|TailMatch], Binds) when is_atom(V) ->
    case lists:keyfind(V, 1, Binds) of
        {_, E} ->
            list_match(Tail, TailMatch, Binds);
        {_, _} ->
            false;
        false ->
            list_match(Tail, TailMatch, [{V, E}|Binds])
    end;
%% Match complete.
list_match([], [], Binds) ->
    {true, Binds, undefined};
%% Values don't match, stop.
list_match(_List, _Match, _Binds) ->
    false.

parse_qs(Query) ->
    KVs = binary:split(Query, <<"&">>, [global,trim]),
    lists:foldl(fun(KV, Acc) -> 
                case binary:split(KV, <<"=">>) of
                    [Key, Value] ->
                        [{Key, Value} | Acc];
                    _ ->
                        Acc
                end
        end, [], KVs).

%% ===================================================================
%% Tests
%% ===================================================================

-ifdef(TEST).

compile_test_() ->
    [
        ?_assertMatch([{[<<"procs">>], [], procs}], compile([{"/procs", procs}])),
        ?_assertMatch([{[<<"procs">>, node], [], procs}], 
            compile([{"/procs/:node", procs}])),
        ?_assertMatch([{[<<"procs">>,'...'], [], procs}], 
            compile([{"/procs/[...]", procs}]))
    ].

match_test_() ->
    [
        ?_assertMatch({procs, _}, match(<<"/procs">>,[{[<<"procs">>], procs}])),
        ?_assertMatch({default, _}, match(<<"/procs/tail">>,[{[<<"procs">>], procs}])),
        ?_assertMatch({procs, _}, match(<<"/procs/tail">>,
                [{[<<"procs">>, <<"tail">>], procs}])),
        ?_assertMatch({procs, _}, match(<<"/procs/tail">>,
                [{[<<"procs">>, '...'], procs}]))
    ].

path_info_test() ->
    {procs, Env} = match(<<"/procs/foo/bar/baz">>, [{[<<"procs">>, '...'], procs}]),
    ?assertMatch([<<"foo">>,<<"bar">>,<<"baz">>], path_info(Env)).

qs_test() ->
    {procs, Env} = match(<<"/procs?foo=bar&baz=huge">>, [{[<<"procs">>], procs}]),
    ?assertMatch(<<"bar">>, qs_val(<<"foo">>, Env)),
    ?assertMatch(<<"huge">>, qs_val(<<"baz">>, Env)).

binding_test() ->
    {procs, Env} = match(<<"/procs/node">>, [{[<<"procs">>, node], procs}]),
    <<"node">> = binding(node, Env).

-endif.
