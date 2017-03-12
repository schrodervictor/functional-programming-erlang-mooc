-module(permutations).
-export([gen/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (public) gen/1
%%
%% Generates all the permutations for the given list
%%
%% The strategy to produce the permutations is to generate all the
%% permutations for the subset of the list, by excluding the first element and
%% then reinserting this element in every possible position in every
%% permutation of the subset.
%%
-spec gen([any()]) -> [[any()]].

gen([]) -> [];
gen([A|List]) -> insert_element(A, gen(List)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (private) insert_element/2
%%
%% Takes care of iterating through all permutations found for the subset,
%% one by one.
%%
-spec insert_element(any(), [[any()]]) -> [[any()]].

insert_element(_, []) -> [];
insert_element(A, [Perm|Perms]) ->
    insert(A, Perm) ++ insert_element(A, Perms).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (private) insert/2
%%
%% Takes care of inserting a single element in to a list in every possible
%% position.
%%
%% Entrypoint for insert/3.
%%
-spec insert(any(), [any()]) -> [[any()]].

insert(A, Perm) -> insert(A, [], Perm).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (private) insert/3
%%
%% Iterator of insert/2.
%%
-spec insert(any(), [any()], [any()]) -> [[any()]].

insert(A, Begin, []) -> [Begin ++ [A]];
insert(A, Begin, [B|End]) ->
    [Begin ++ [A, B|End] | insert(A, [B|Begin], End)].
