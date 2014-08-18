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
         flatlength/1,
         flatmap/2,
         flatten/1,
         flatten/2,
         foldl/3,
         foldr/3,
         foreach/2,
         keydelete/3,
         keyfind/3,
         keymap/3,
         keymember/3,
         keymerge/3,
         keyreplace/4,
         keysearch/3,
         keysort/2,
         keystore/4,
         keytake/3,
         last/1,
         map/2,
         mapfoldl/3,
         mapfoldr/3,
         max/1,
         member/2,
         merge/1]).
-export([get_reverse_list/2,
         for_merge/2]).

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


flatmap(_, []) -> [];
flatmap(Fun, [First|Rest]) ->
    append(Fun(First), flatmap(Fun, Rest)).

flatten([]) -> [];
flatten([First|Rest]) when is_list(First)->
    append(flatten(First), flatten(Rest));
flatten([First|Rest]) ->
    [First | flatten(Rest)].

flatten(DeepList, Tail) ->
    append(flatten(DeepList), Tail).


foldl(_, Acc, []) -> Acc;
foldl(Fun, Acc0, [First|Rest]) -> foldl(Fun, Fun(First, Acc0), Rest).


foldr(Fun, Acc0, List) ->
    foldl(Fun, Acc0, reverse(List)).

foreach(_, []) -> ok;
foreach(Fun, [First|Rest]) ->
    Fun(First),
    foreach(Fun, Rest).


keydelete(_Key, _N, []) -> [];
keydelete(Key, N, [First|Rest]) when is_tuple(First), N>=1->
    try element(N, First) == Key of
        true ->
            Rest;
        false ->
            [First | keydelete(Key, N, Rest)]
    catch
        error:badarg ->
            [First | keydelete(Key, N, Rest)]
    end.


keyfind(_Key, _N, []) -> false;
keyfind(Key, N, [First|Rest]) when is_tuple(First), N>=1 ->
    try element(N, First) == Key of
        true ->
            First;
        false ->
            keyfind(Key, N, Rest)
    catch
        error:badarg ->
            keyfind(Key, N, Rest)
    end.


keymap(_Fun, _N, []) ->[];
keymap(Fun, N ,[First|Rest]) when is_tuple(First), N>=1 ->
    L = length(tuple_to_list(First)),
    Func = fun(K, Acc0) ->
                   Temp = fun(This, I, Acc) ->
                                  case I of
                                      N ->
                                          This(This, I+1, erlang:append_element(Acc, Fun(element(I, First))));
                                      _ when I == L+1 ->
                                          Acc;
                                      _ ->
                                          This(This, I+1, erlang:append_element(Acc, element(I, First)))
                                  end
                          end,
                   Temp(Temp, K, Acc0)
           end,
    [Func(1, {}) | keymap(Fun, N, Rest)].


keymember(_Key, _N, []) -> false;
keymember(Key, N, [First|Rest]) when N>=1, is_tuple(First) ->
    try element(N ,First) of
        V when V == Key ->
            true;
        _ ->
            keymember(Key, N, Rest)
    catch
        error:badarg ->
            keymember(Key, N, Rest)
    end.

keymerge(_N, List1, []) -> List1;
keymerge(_N, [], List2) -> List2;
keymerge(N, [F1|R1], [F2|R2])
  when N>=1, is_tuple(F1), is_tuple(F2),
       element(N, F1) =< element(N, F2) ->
    [F1 | keymerge(N, R1, [F2|R2])];
keymerge(N, List1, [F2|R2])
  when N>=1, is_tuple(F2) ->
    _ = element(N, F2),
    [F2 | keymerge(N, List1, R2)].


keyreplace(_Key, _N, [], _NewTuple) -> [];
keyreplace(Key, N, [First|Rest], NewTuple) when N>=1,is_tuple(First) ->
    try element(N, First) of
        K when K == Key ->
            [NewTuple | Rest];
        _Other ->
            [First | keyreplace(Key, N, Rest, NewTuple)]
    catch
        error:badarg ->
            [First | keyreplace(Key, N, Rest, NewTuple)]
    end.


keysearch(_Key, _N, []) -> false;
keysearch(Key, N, [First|Rest]) when N>=1, is_tuple(First) ->
    try element(N, First) of
        K when K == Key ->
            {value, First};
        _Other ->
            keysearch(Key, N, Rest)
    catch
        error:badarg ->
            keysearch(Key, N, Rest)
    end.


keysort(_N, []) -> [];
keysort(N, [First|Rest]) when N>=1, is_tuple(First)->
    Left = filter(fun(Elem)->element(N, Elem)<element(N, First) end, Rest),
    Right = filter(fun(Elem)->element(N, Elem)>=element(N, First) end, Rest),
    append([keysort(N, Left), [First], keysort(N, Right)]).



keystore(_Key, _N, [], NewTuple) when is_tuple(NewTuple)-> [NewTuple];
keystore(Key, N, [First|Rest], NewTuple) when N>=1, is_tuple(NewTuple), is_tuple(First) ->
    try element(N ,First) of
        V when V == Key ->
            [NewTuple | Rest];
        _Other ->
            [First | keystore(Key, N, Rest, NewTuple)]
    catch
        error:badarg ->
            [First | keystore(Key, N, Rest, NewTuple)]
    end.


keytake(Key, N, List) when N>=1 ->
    case keyfind(Key, N, List) of
        false ->
            false;
        Tuple when is_tuple(Tuple)->
            {value, Tuple, keydelete(Key, N, List)};
        _ ->
            error(wrong)
    end.


last([Aim]) -> Aim;
last([_First|Rest]) -> last(Rest).


map(_Fun, []) ->[];
map(Fun, [First|Rest]) -> [Fun(First) |map(Fun, Rest)].


mapfoldl(_Fun, Acc0, []) -> {[], Acc0};
mapfoldl(Fun, Acc0, [First|Rest]) ->
    {B, AccOut} = Fun(First, Acc0),
    {List, Acc} = mapfoldl(Fun, AccOut, Rest),
    {[B | List], Acc}.


mapfoldr(Fun, Acc0, List) ->
    Rlist = reverse(List),
    {A,B} = mapfoldl(Fun, Acc0, Rlist),
    {reverse(A), B}.


for_max(Res, []) -> Res;
for_max(Res, [First|Rest]) when First>Res ->
    for_max(First, Rest);
for_max(Res, [First|Rest]) when First=<Res ->
    for_max(Res, Rest).

max([First|Rest]) ->
    for_max(First, Rest).


member(_Elem, []) -> false;
member(Elem, [First|_Rest]) when Elem =:= First -> true;
member(Elem, [_First|Rest]) -> member(Elem, Rest).


for_merge(Min, []) -> {Min, []};
for_merge(Min, [First|Rest]) when length(First)>0 ->
    [FirstElem|RestElems] = First,
    case FirstElem < Min of
        true ->
            {MinElem, ResLists} = for_merge(FirstElem, Rest),
            case MinElem /= FirstElem of
                true ->
                    {MinElem, [First | ResLists]};
                false ->
                    {MinElem, [RestElems | ResLists]}
            end;
        false ->
            {MinElem, ResLists} = for_merge(Min, Rest),
            {MinElem, [First | ResLists]}
    end;
for_merge(Min, [_First|Rest]) ->
    {MinElem, ResLists} = for_merge(Min, Rest),
    {MinElem, [[] | ResLists]}.

merge([]) -> [];
merge([[]|Rest]) -> merge(Rest);
merge([[FirstElem|RestElems]|Rest]) ->
    {Min, ResLists} = for_merge(FirstElem+1, [[FirstElem|RestElems]|Rest]),
    [Min | merge(ResLists)].
