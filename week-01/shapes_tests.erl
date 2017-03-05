-module(shapes_tests).
-include("shapes.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PRECISION, 0.000001).

-define(
    _assertFloat(Num0, Num1),
    fun() -> ?assert(compareFloats(Num0, Num1)) end
).

-define(
    _assertRectangle(Points, Rectangle),
    fun() -> checkRectangle(Points, Rectangle) end
).

%% If the number is too small in absolute value, it is indistinguishable from
%% zero, so we compare the other number with the precision itself (and we
%% avoid possible divisions by zero)
compareFloats(Num0, Num1) when abs(Num1) < ?PRECISION ->
    abs(Num0) < ?PRECISION;

%% Classical comparison, within a pre-defined precision
%% (given that the second number is not zero)
compareFloats(Num0, Num1) ->
    abs(1 - Num0/Num1) < ?PRECISION.

%% Comparison of points is achieved by simply comparing the coordinates
comparePoints(#point{x=X0, y=Y0}, #point{x=X1, y=Y1}) ->
    compareFloats(X0, X1) andalso compareFloats(Y0, Y1).

%% Assertion about rectangles is done is two steps. First it has to be a
%% rectangle by definition. ?assertMatch(#rectangle{}, _) does this check.
%% Then, because a rectangle is defined by three points in the 2D space, the
%% vertices might be any combination of the four possible points, in any
%% order, so we iterate through all the vertices, checking them against a list
%% of all possible points.
checkRectangle(PossiblePoints, Rectangle) ->
    ?assertMatch(#rectangle{}, Rectangle),
    ?assert(checkRectanglePoints(PossiblePoints, Rectangle)).

checkRectanglePoints(
    PossiblePoints,
    Rec = #rectangle{vertex0=V0, vertex1=V1, vertex2=V2}
) ->
    io:format("Comparing: ~p and ~p~n", [PossiblePoints, Rec]),
    checkRectanglePoints(PossiblePoints, [V0, V1, V2]);


checkRectanglePoints(_, []) ->
    %% The vertices list is empty, which means all of them matched some of
    %% the points in the possible points list. This is a good rectangle.
    true;

checkRectanglePoints(PossiblePoints, [Vertex|Vertices]) ->
    Result = lists:partition(
        fun(Point) ->
            Res = comparePoints(Point, Vertex),
            io:format("Res ~p~n", [Res]),
            Res
        end,
        PossiblePoints
    ),
    io:format("Result: ~p~n", [Result]),
    {Match, RestPoints} = Result,
    %% Every point in the rectangle has to match one point in the list of
    %% possible points. That's why the Match var should never be empty at
    %% this point, after a list partition. The partition is necessary because
    %% we don't want repeated matches.
    ?assertNotEqual([], Match),
    checkRectanglePoints(RestPoints, Vertices).


test_cases() -> [
    #square{
        vertex0=#point{x=1.0, y=1.0},
        vertex2=#point{x=0.0, y=0.0}
    },
    #square{
        vertex0=#point{x=1.0, y=1.0},
        vertex2=#point{x=5.0, y=5.0}
    },
    #square{
        vertex0=#point{x=7.0, y=0.0},
        vertex2=#point{x=7.0, y=8.0}
    },
    #rectangle{
        vertex0=#point{x=0.0, y=0.0},
        vertex1=#point{x=7.0, y=0.0},
        vertex2=#point{x=7.0, y=3.0}
    },
    #rectangle{
        vertex0=#point{x=0.0, y=0.0},
        vertex1=#point{x=7.0, y=7.0},
        vertex2=#point{x=4.0, y=10.0}
    },
    #rectangle{
        vertex0=#point{x=-10.0, y=-5.0},
        vertex1=#point{x=10.0, y=0.0},
        vertex2=#point{x=9.0, y=4.0}
    },
    #circle{
        center=#point{x=0, y=0},
        radius=1
    },
    #circle{
        center=#point{x=1000000, y=0.0000004},
        radius=100
    },
    #triangle{
        vertex0=#point{x=0, y=0},
        vertex1=#point{x=3, y=0},
        vertex2=#point{x=3, y=4}
    },
    #triangle{
        vertex0=#point{x=-50, y=3},
        vertex1=#point{x=-2, y=0},
        vertex2=#point{x=20, y=1}
    }
].

perimeters() -> [
    4.0,
    16.0,
    22.6274,
    20.0,
    28.28428,
    49.4773,
    6.283185307,
    628.3185307,
    12,
    140.145
].

areas() -> [
    1,
    16,
    32,
    21,
    42,
    85,
    3.141592653,
    31415.92653,
    6,
    57
].

enclosing_rectangles() -> [
    [
        #point{x=0.0, y=0.0},
        #point{x=1.0, y=0.0},
        #point{x=1.0, y=1.0},
        #point{x=0.0, y=1.0}
    ],
    [
        #point{x=1.0, y=1.0},
        #point{x=5.0, y=5.0},
        #point{x=5.0, y=1.0},
        #point{x=1.0, y=5.0}
    ],
    [
        #point{x=7.0, y=0.0},
        #point{x=11.0, y=4.0},
        #point{x=7.0, y=8.0},
        #point{x=3.0, y=4.0}
    ],
    [
        #point{x=0.0, y=0.0},
        #point{x=7.0, y=0.0},
        #point{x=7.0, y=3.0},
        #point{x=0.0, y=3.0}
    ],
    [
        #point{x=0.0, y=0.0},
        #point{x=7.0, y=7.0},
        #point{x=4.0, y=10.0},
        #point{x=-3.0, y=3.0}
    ],
    [
        #point{x=-10.0, y=-5.0},
        #point{x=10.0, y=0.0},
        #point{x=9.0, y=4.0},
        #point{x=-11.0, y=-1.0}

    ],
    [
        #point{x=-0.5, y=-0.5},
        #point{x=0.5, y=-0.5},
        #point{x=0.5, y=0.5},
        #point{x=-0.5, y=0.5}
    ],
    [
        #point{x=999950, y=-49.9999996},
        #point{x=1000050, y=-49.9999996},
        #point{x=1000050, y=50.0000004},
        #point{x=999950, y=50.0000004}
    ],
    [
        #point{x=0, y=0},
        #point{x=3, y=0},
        #point{x=3, y=4},
        #point{x=0, y=4}
    ],
    [
        #point{x=-50, y=3},
        #point{x=-50.046492, y=1.372756},
        #point{x=20, y=1},
        #point{x=19.953508, y=-0.627244}
    ]
].

perimeter_test_() ->
    [
        ?_assertFloat(ExpectedPerimeter, shapes:perimeter(Shape))
        ||
        {Shape, ExpectedPerimeter} <- lists:zip(test_cases(), perimeters())
    ].

area_test_() ->
    [
        ?_assertFloat(ExpectedArea, shapes:area(Shape))
        ||
        {Shape, ExpectedArea} <- lists:zip(test_cases(), areas())
    ].

enclosing_rectangle_test_() ->
    [
        ?_assertRectangle(PossiblePoints, shapes:enclose(Shape))
        ||
        {Shape, PossiblePoints} <- lists:zip(test_cases(), enclosing_rectangles())
    ].
