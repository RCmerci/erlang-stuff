-module(implement_stdlib_list).
-export([all/2,
         any/2,
         append/1,
         append/2,
         reverse/1,
         concat/1,
         delete/2,
         dropwhile/2,
         duplicate/2,
         filter/2,
         filtermap/2,
         flatlength/1]).
-export([get_reverse_list/2]).

all(_, []) -> true;
all(Pred, [First|Rest]) ->
    case Pred(First) of
        true ->
            all(Pred, Rest);
        false ->
            false;
        _ ->
            io:format("Pred:~p should return boolean() ~n", [Pred])
    end.


any(_, []) -> false;
any(Pred, [First|Rest]) ->
    case Pred(First) of
        true ->
            true;
        false ->
            any(Pred, Rest);
        _ ->
            io:format("Pred:~p should return boolean()~n ", [Pred])
    end.


get_reverse_list(Reversed, []) -> Reversed;
get_reverse_list(Reversed, Source) ->
    [First|Rest] = Source,
    get_reverse_list([First|Reversed], Rest).

reverse(List1) ->
    get_reverse_list([], List1).

for_append([], Res) -> Res;
for_append(Rlist1, Res) ->
    [First|Rest] = Rlist1,
    for_append(Rest, [First|Res]).

append(List1, List2) ->
    ReverseList1 = reverse(List1),
    for_append(ReverseList1, List2).

append([First, Sec | Rest]) ->
    append([append(First, Sec)| Rest]);
append([Result]) -> Result.

for_concat([]) -> [];
for_concat(Things) ->
    [First|Rest] = Things,
    if
        is_atom(First) ->
            [atom_to_list(First)|for_concat(Rest)];
        is_integer(First) ->
            [integer_to_list(First)|for_concat(Rest)];
        is_float(First)->
            [float_to_list(First)|for_concat(Rest)];
        is_list(First) ->
            [First|for_concat(Rest)];
        true ->
            exit('The elements of Things can be atoms, integers,floats or strings')
    end.

concat(Things) ->
    ThingsAllList = for_concat(Things),
    append(ThingsAllList).

delete(_, []) -> [];
delete(Elem, [First|Rest]) when First =:= Elem -> Rest;
delete(Elem, [First|Rest]) when First =/= Elem ->
    [First|delete(Elem, Rest)].


dropwhile(Pred, [First|Rest]) ->
    case Pred(First) of
        true ->
            dropwhile(Pred, Rest);
        _ ->
            [First|Rest]
    end.


duplicate(N, _Elem) when N == 0-> [];
duplicate(N, Elem) when N > 0->
    [Elem | duplicate(N-1, Elem)].


filter(_, []) -> [];
filter(Pred, [First|Rest]) ->
    case Pred(First) of
        true ->
            [First | filter(Pred, Rest)];
        _ ->
            filter(Pred, Rest)
    end.

filtermap(_, []) -> [];
filtermap(Fun, [First|Rest]) ->
    case Fun(First) of
        true ->
            [First | filtermap(Fun, Rest)];
        {true, Value} ->
            [Value | filtermap(Fun, Rest)];
        false ->
            filtermap(Fun, Rest)
    end.

flatlength([]) -> 0;
flatlength([First|Rest]) when is_list(First) -> flatlength(First) + flatlength(Rest);
flatlength([_|Rest]) -> 1 + flatlength(Rest).
