-type length() :: float().

-record(point, {x :: float(), y :: float()}).
-type point() :: #point{}.

-record(segment, {p0 :: point(), p1 :: point()}).
-type segment() :: #segment{}.

% A rectangle is fully qualified in 2D space by three vertices
-record(rectangle, {
    vertex0 :: point(),
    vertex1 :: point(),
    vertex2 :: point()
}).
-type rectangle() :: #rectangle{}.

% A square is fully qualified in 2D space by two opposite vertices
-record(square, {
    vertex0 :: point(),
    vertex2 :: point()
}).
-type square() :: #square{}.

% A circle is fully qualified in 2D space by its center and radius
-record(circle, {
    center :: point(),
    radius :: length()
}).
-type circle() :: #circle{}.

% A triangle is fully qualified in 2D space by its vertices
-record(triangle, {
    vertex0 :: point(),
    vertex1 :: point(),
    vertex2 :: point()
}).
-type triangle() :: #triangle{}.

-type shape() :: square() | rectangle() | circle() | triangle().
