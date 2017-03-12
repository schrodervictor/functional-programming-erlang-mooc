-module(my_lists).
-export([join/2, concat/1, member/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (public) join/2
%%
%% Joins two lists together.
%%
-spec join(list(), list()) -> list().

join([], List) -> List;
join([Elem|Begin], End) -> [Elem|join(Begin, End)].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (public) concat/1
%%
%% Joins several lists together. They have to be passed in a list.
%%
-spec concat([list()]) -> list().

concat([]) -> [];
concat([List|Lists]) -> join(List, concat(Lists)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (public) member/2
%%
%% Checks if a element is a member of a list.
%%
-spec member(any(), list()) -> boolean().

member(_, []) -> false;
member(Elem, [Elem|_]) -> true;
member(Elem, [_|Tail]) -> member(Elem, Tail).
