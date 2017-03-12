-module(index_tests).
-include_lib("eunit/include/eunit.hrl").

main_simple_test() ->
    Lines = [
        "foo",
        "bar",
        "bar",
        "foo",
        "foo",
        "foo bar",
        "foo bar foo",
        "foobar",
        "bar",
        "foo bar"
    ],
    Expected = [
        {"bar", [{1,2}, {5,6}, {8,9}]},
        {"foo", [{0,0}, {3,6}, {9,9}]},
        {"foobar", [{7,7}]}
    ],
    ?assertEqual(Expected, index:main(Lines)).

main_ponctuation_test() ->
    Lines = [
        "foo   ",
        "bar, ; !",
        "bar",
        "foo",
        "foo",
        "foo -- bar!",
        "foo; bar? foo^",
        "foobar/",
        "&bar",
        "foo bar."
    ],
    Expected = [
        {"bar", [{1,2}, {5,6}, {8,9}]},
        {"foo", [{0,0}, {3,6}, {9,9}]},
        {"foobar", [{7,7}]}
    ],
    ?assertEqual(Expected, index:main(Lines)).

main_lower_upper_test() ->
    Lines = [
        "Foo   ",
        "bAR, ; !",
        "bar",
        "foo",
        "foO",
        "foo -- bar!",
        "Foo; bar? foo^",
        "fOObar/",
        "&baR",
        "foo Bar."
    ],
    Expected = [
        {"bar", [{1,2}, {5,6}, {8,9}]},
        {"foo", [{0,0}, {3,6}, {9,9}]},
        {"foobar", [{7,7}]}
    ],
    ?assertEqual(Expected, index:main(Lines)).

main_remove_small_words_test() ->
    Lines = [
        "of a Foo   ",
        "bAR, in; on!",
        "bar",
        "foo",
        "foO Ah!",
        "foo -- bar!",
        "Foo; uh? bar? foo^",
        "fOObar/aa",
        "&baR",
        "foo Bar."
    ],
    Expected = [
        {"bar", [{1,2}, {5,6}, {8,9}]},
        {"foo", [{0,0}, {3,6}, {9,9}]},
        {"foobar", [{7,7}]}
    ],
    ?assertEqual(Expected, index:main(Lines)).
