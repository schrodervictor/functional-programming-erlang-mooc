-module(sum_bits).
-export([bits/1, bits_tr/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (public) bits/1
%%
-spec bits(integer()) -> integer().

bits(0) -> 0;
bits(N) when 0 =:= (N rem 2) -> bits(N div 2);
bits(N) -> 1 + bits(N - 1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (public) bits_tr/1
%%
%% Tail recursive version of bits/1
%%
-spec bits_tr(integer()) -> integer().

bits_tr(N) -> bits_tr(N, 0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (private) find_base/2
%%
-spec bits_tr(integer(), integer()) -> integer().

bits_tr(0, Acc) -> Acc;
bits_tr(N, Acc) when 0 =:= (N rem 2) -> bits_tr(N div 2, Acc);
bits_tr(N, Acc) -> bits_tr(N - 1, Acc + 1).
