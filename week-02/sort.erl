-module(sort).
-export([merge_sort/1, quick_sort/1, insert_sort/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (public) merge_sort/1
%%
-spec merge_sort([any()]) -> [any()].

merge_sort([]) -> [];
merge_sort([A]) -> [A];
merge_sort(List) ->
    {A, B} = split_in_two(List),
    merge_sorted(merge_sort(A), merge_sort(B)).

split_in_two(List) -> split_in_two([], List).

split_in_two(Acc, List) when length(Acc) >= length(List) -> {Acc, List};
split_in_two(Acc, [A|List]) -> split_in_two([A|Acc], List).

merge_sorted(OrdListA, []) -> OrdListA;
merge_sorted([], OrdListB) -> OrdListB;
merge_sorted([A|OrdListA], [B|OrdListB]) ->
    case A > B of
        true -> [B|merge_sorted([A|OrdListA], OrdListB)];
        false -> [A|merge_sorted(OrdListA, [B|OrdListB])]
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (public) quick_sort/1
%%
-spec quick_sort([any()]) -> [any()].

quick_sort([]) -> [];
quick_sort([A]) -> [A];
quick_sort([Elem|List]) ->
    {A, B} = pivot_split(Elem, List),
    quick_sort(A) ++ [Elem] ++ quick_sort(B).

pivot_split(Pivot, List) -> pivot_split(Pivot, List, {[], []}).

pivot_split(_, [], Acc) -> Acc;
pivot_split(Pivot, [Elem|List], {Smaller, Larger}) ->
    case Elem =< Pivot of
        true -> pivot_split(Pivot, List, {[Elem|Smaller], Larger});
        false -> pivot_split(Pivot, List, {Smaller, [Elem|Larger]})
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (public) insert_sort/1
%%
-spec insert_sort([any()]) -> [any()].

insert_sort([]) -> [];
insert_sort([Head|Tail]) ->
    insert(Head, insert_sort(Tail)).

insert(Elem, []) -> [Elem];
insert(Elem, [Head|OrdList]) ->
    case Elem =< Head of
        true -> [Elem, Head|OrdList];
        false -> [Head|insert(Elem, OrdList)]
    end.
