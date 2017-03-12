-module(permutations_tests).
-include_lib("eunit/include/eunit.hrl").

-define(
    _assertLengthAndContents(Expected, List),
    fun() -> assert_length_and_contents(Expected, List) end
).

assert_length_and_contents(Expected, List) ->
    length(Expected) =:= length(List) andalso assert_contents(Expected, List).

assert_contents([], _) -> true;
assert_contents([Item|Expected], List) ->
    lists:member(Item, List) andalso assert_contents(Expected, List).


permutation_gen_test_() ->
    Cases = [
        {[], []},
        {[1], [[1]]},
        {[1,2], [[1,2], [2,1]]},
        {[1,1], [[1,1], [1,1]]},
        {[1,2,3], [
            [1,2,3], [1,3,2],
            [2,1,3], [2,3,1],
            [3,1,2], [3,2,1]
        ]},
        {[1,2,3,4], [
            [1,2,3,4], [1,2,4,3], [1,3,2,4], [1,3,4,2], [1,4,2,3], [1,4,3,2],
            [2,1,3,4], [2,1,4,3], [2,3,1,4], [2,3,4,1], [2,4,1,3], [2,4,3,1],
            [3,1,2,4], [3,1,4,2], [3,2,1,4], [3,2,4,1], [3,4,1,2], [3,4,2,1],
            [4,1,2,3], [4,1,3,2], [4,2,1,3], [4,2,3,1], [4,3,1,2], [4,3,2,1]
        ]}
    ],
    [
        ?_assertLengthAndContents(Expected, permutations:gen(Input))
        ||
        {Input, Expected} <- Cases
    ].
