-module(my_lists_tests).
-include_lib("eunit/include/eunit.hrl").

-import(my_lists, [join/2, concat/1, member/2]).

join_test_() ->
    [
        ?_assertEqual([], join([], [])),
        ?_assertEqual("hello", join("he", "llo"))
    ].

concat_test_() ->
    [
        ?_assertEqual([], concat([[], [], []])),
        ?_assertEqual("hello world!", concat(["", "hel", "lo world", "!"]))
    ].

member_test_() ->

    List = [1, 3, 999, a, "foo", {some, tuple}],
    [
        ?_assert(member(1, List)),
        ?_assert(member(3, List)),
        ?_assert(member(999, List)),
        ?_assert(member(a, List)),
        ?_assert(member("foo", List)),
        ?_assert(member([$f, $o, $o], List)),
        ?_assert(member({some, tuple}, List)),

        ?_assertNot(member(2, List)),
        ?_assertNot(member(ab, List)),
        ?_assertNot(member("bar", List)),
        ?_assertNot(member([$f, $o], List)),
        ?_assertNot(member({another, tuple}, List))
    ].
