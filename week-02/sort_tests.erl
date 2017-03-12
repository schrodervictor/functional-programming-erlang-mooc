-module(sort_tests).
-include_lib("eunit/include/eunit.hrl").

sort_test_() ->
    Algorithms = [
        fun sort:merge_sort/1,
        fun sort:quick_sort/1,
        fun sort:insert_sort/1
    ],
    Cases = [
        {[], []},
        {[1], [1]},
        {[1,2,3], [1,2,3]},
        {[1,3,2,9,4,1], [1,1,2,3,4,9]},
        {[1,1,1,1,8,1], [1,1,1,1,1,8]},
        {[-1,-4,-1000,100], [-1000,-4,-1,100]}
    ],
    [
        ?_assertEqual(Expected, Sort(Input))
        ||
        Sort <- Algorithms,
        {Input, Expected} <- Cases
    ].

