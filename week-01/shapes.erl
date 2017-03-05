-module(shapes).
-export([perimeter/1, area/1, enclose/1]).

-include("shapes.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (public) perimeter/1
%%
-spec perimeter(shape()) -> float().

perimeter(#square{vertex0=V0, vertex2=V2}) ->
    2.0*math:sqrt(2)*distance(V0, V2);

perimeter(#rectangle{vertex0=V0, vertex1=V1, vertex2=V2}) ->
    2.0*(distance(V0, V1) + distance(V1, V2));

perimeter(#circle{radius=Radius}) ->
    2.0*math:pi()*Radius;

perimeter(#triangle{vertex0=V0, vertex1=V1, vertex2=V2}) ->
    distance(V0, V1) + distance(V1, V2) + distance(V2, V0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (public) area/1
%%
-spec area(shape()) -> float().

area(#square{vertex0=V0, vertex2=V2}) ->
    math:pow(distance(V0, V2), 2)/2;

area(#rectangle{vertex0=V0, vertex1=V1, vertex2=V2}) ->
    distance(V0, V1)*distance(V1, V2);

area(#circle{radius=Radius}) ->
    math:pi()*math:pow(Radius, 2);

area(Triangle = #triangle{vertex0=V0, vertex1=V1, vertex2=V2}) ->
    S = perimeter(Triangle)/2,
    Side0 = distance(V0, V1),
    Side1 = distance(V1, V2),
    Side2 = distance(V2, V0),
    math:sqrt(S*(S - Side0)*(S - Side1)*(S - Side2)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (public) enclose/1
%%
-spec enclose(shape()) -> rectangle().

enclose(#square{vertex0=#point{x=X0, y=Y0}, vertex2=#point{x=X2, y=Y2}}) ->
    DeltaX = X2 - X0,
    DeltaY = Y2 - Y0,
    #rectangle{
        vertex0=#point{x=X0, y=Y0},
        vertex1=#point{
            x=X0 + (DeltaX + DeltaY)/2,
            y=Y0 + (DeltaY - DeltaX)/2
        },
        vertex2=#point{x=X2, y=Y2}
    };

enclose(Rectangle = #rectangle{}) -> Rectangle;

enclose(#circle{center=#point{x=X, y=Y}, radius=Radius}) ->
    #rectangle{
        vertex0=#point{x=X - Radius/2, y=Y - Radius/2},
        vertex1=#point{x=X + Radius/2, y=Y - Radius/2},
        vertex2=#point{x=X + Radius/2, y=Y + Radius/2}
    };

enclose(Triangle = #triangle{}) -> enclose(Triangle, 3, {null, null, null}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (private) enclose/3
%%
-spec enclose(shape(), integer(), {integer(), segment(), length()}) ->
    rectangle().

%% Generic abstraction to get the best rectangle from the collection of
%% base segments and repective heights. Will work for any convex shape with
%% minimal adjustments
enclose(_Shape, 0, {_Area, Base, Height}) ->
    #segment{p0=P0, p1=P1} = Base,
    DeltaX = P1#point.x - P0#point.x,
    DeltaY = P1#point.y - P0#point.y,
    Lenght = segment_length(Base),
    X = P1#point.x - (DeltaY/Lenght)*Height,
    Y = P1#point.y + (DeltaX/Lenght)*Height,
    #rectangle{vertex0=P0, vertex1=P1, vertex2=#point{x=X, y=Y}};

%% Shape-specific recursion to collect all bases and heights
%% Only triangle is implemented so far
enclose(Triangle = #triangle{}, Count, {MinArea, MinBase, MinHeight}) ->
    #triangle{vertex0=V0, vertex1=V1, vertex2=V2} = Triangle,
    Base = find_base(V2, #segment{p0=V0, p1=V1}),
    Length = segment_length(Base),
    Height = distance(V2, Base),
    Area = Length*Height,
    io:format("Base to Vertex: ~p ~p~n", [Base, Height]),
    NewMin = case Area < MinArea of
        true -> {Area, Base, Height};
        false -> {MinArea, MinBase, MinHeight}
    end,
    NextTriangle = #triangle{vertex0=V1, vertex1=V2, vertex2=V0},
    enclose(NextTriangle, Count - 1, NewMin).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (private) distance/2
%%
-spec distance(point(), point()) -> length();
              (point(), segment()) -> length().

distance(#point{x=X0, y=Y0}, #point{x=X1, y=Y1}) ->
    math:sqrt(math:pow(X0 - X1, 2) + math:pow(Y0 - Y1, 2));

distance(
    #point{x=XP, y=YP},
    #segment{p0=#point{x=X0, y=Y0}, p1=#point{x=X1, y=Y1}}
) ->
    DeltaX = X1 - X0,
    DeltaY = Y1 - Y0,
    abs(DeltaY*XP  - DeltaX*YP + X1*Y0 - X0*Y1) /
        math:sqrt(math:pow(DeltaX, 2) + math:pow(DeltaY, 2)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (private) segment_length/2
%%
-spec segment_length(segment()) -> length().

segment_length(#segment{p0=P0, p1=P1}) -> distance(P0, P1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (private) find_base/2
%%
-spec find_base(point(), segment()) -> segment().

find_base(
    #point{x=XP},
    #segment{p0=#point{x=X0, y=Y}, p1=#point{x=X1, y=Y}}
) ->
    Xs = [X0, X1, XP],
    #segment{
        p0=#point{x=lists:min(Xs), y=Y},
        p1=#point{x=lists:max(Xs), y=Y}
    };

find_base(
    #point{y=YP},
    #segment{p0=#point{x=X, y=Y0}, p1=#point{x=X, y=Y1}}
) ->
    Ys = [Y0, Y1, YP],
    #segment{
        p0=#point{x=X, y=lists:min(Ys)},
        p1=#point{x=X, y=lists:max(Ys)}
    };

find_base(#point{x=XP, y=YP}, #segment{p0=P0, p1=P1}) ->
    #point{x=X0, y=Y0} = P0,
    #point{x=X1, y=Y1} = P1,

    Slope = (Y1 - Y0)/(X1 - X0),
    SlopePerp = -1/Slope,

    X = ((Y0 - Slope*X0) - (YP - SlopePerp*XP))/(SlopePerp - Slope),
    Y = Slope*(X - X0) + Y0,
    P = #point{x=X, y=Y},
    lists:foldl(
        fun(Segment, MaxSegment) ->
            case segment_length(Segment) > segment_length(MaxSegment) of
                true -> Segment;
                false -> MaxSegment
            end
        end,
        #segment{p0=P0, p1=P1},
        [#segment{p0=P0, p1=P}, #segment{p0=P, p1=P1}]
    ).
