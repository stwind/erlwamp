-module(wamp_util).

-export([json_encode/1]).
-export([json_decode/1]).
-export([uid62/0]).

%% ===================================================================
%% Public
%% ===================================================================

json_encode(Term) ->
    jiffy:encode(Term).

json_decode(Raw) ->
    jiffy:decode(Raw).

uid62() ->
    Rand = crypto:sha(term_to_binary({make_ref(), now()})),
    <<I:160/integer>> = Rand,
    list_to_binary(int_to_list(I, 62)).

%% ===================================================================
%% Private
%% ===================================================================

int_to_list(I, 10) ->
    erlang:integer_to_list(I);
int_to_list(I, Base)
  when is_integer(I), is_integer(Base),Base >= 2, Base =< 1+$Z-$A+10+1+$z-$a ->
    if I < 0 ->
            [$-|int_to_list(-I, Base, [])];
       true ->
            int_to_list(I, Base, [])
    end;
int_to_list(I, Base) ->
    erlang:error(badarg, [I, Base]).

-spec int_to_list(integer(), integer(), string()) -> string().
int_to_list(I0, Base, R0) ->
    D = I0 rem Base,
    I1 = I0 div Base,
    R1 = if D >= 36 ->
            [D-36+$a|R0];
        D >= 10 ->
            [D-10+$A|R0];
        true ->
            [D+$0|R0]
    end,
    if I1 =:= 0 ->
            R1;
        true ->
            int_to_list(I1, Base, R1)
    end.
