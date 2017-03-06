-module(sum_bits_tests).
-include_lib("eunit/include/eunit.hrl").

-import(sum_bits, [bits/1, bits_tr/1]).

test_cases() ->
    [
    %   {Bits, N},
        {1, 1},
        {3, 7},
        {1, 8},
        {13, 7586747665}
    ].


bits_test_() ->
    [?_assertEqual(Bits, bits(N)) || {Bits, N} <- test_cases()].

bits_tr_test_() ->
    [?_assertEqual(Bits, bits_tr(N)) || {Bits, N} <- test_cases()].
