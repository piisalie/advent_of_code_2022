-module(part_2).
-export([run/1]).
-include_lib("eunit/include/eunit.hrl").

%% build a coordinate plane
%% traverse each point
%% compute scenic score (num trees in each direction multiplied)

run_test() -> 8 = run("test_input.txt").

run(InputFile) ->
    {ok, Input} = file:read_file(InputFile),
    Forrest = parse(Input),
    Points = maps:keys(element(3, Forrest)),
    lists:max(lists:map(fun(Coordinate) -> score_visible(Coordinate, Forrest) end, Points)).

parse_line_test() -> ?assert(parse(<<"31\n">>) =:= {1, 0, #{{0,0} => 3, {1,0} => 1}}).

parse(Input) ->
    parse(Input, 0, 0, 0, 0, maps:new()).

parse(<<"\n", Rest/binary>>, X, Y, _Max_x, Max_y, Forrest) ->
    parse(Rest, 0, Y + 1, X, Max_y, Forrest);
parse(<<Tree:1/binary, Rest/binary>>, X, Y, Max_x, Max_y, Forrest) ->
    NewForrest = maps:put({X, Y}, binary_to_integer(Tree), Forrest),
    parse(Rest, X + 1, Y, Max_x, Max_y, NewForrest);
parse(<<"">>, _X, Y, Max_x, _Max_y, Forrest) ->
    % 0 indexes are the best!
    {Max_x - 1, Y - 1, Forrest}.

score_visible_test() ->
    {ok, Input} = file:read_file("test_input.txt"),
    Forrest = parse(Input),
    4 = score_visible({2,1}, Forrest),
    8 = score_visible({2,3}, Forrest).

score_visible({X, _Y}, {Max_x, _Max_y, _Forrest}) when (X == Max_x) or (X == 0) ->
    0;
score_visible({_X, Y}, {_Max_x, Max_y, _Forrest}) when (Y == Max_y) or (Y == 0) ->
    0;
score_visible({X, Y} = Coordinate, Forrest) ->
    Height = maps:get(Coordinate, element(3, Forrest)),

    Above = count_visible_trees({X, Y - 1}, Height, above, Forrest),
    Below = count_visible_trees({X, Y + 1}, Height, below, Forrest),
    Left = count_visible_trees({X - 1, Y}, Height, left, Forrest),
    Right = count_visible_trees({X + 1, Y}, Height, right, Forrest),

    Above * Below * Left * Right.

count_visible_trees(Coordinate, Height, above, Forest) ->
    count_visible_trees(Coordinate, Height, above, 0, 0, Forest);
count_visible_trees(Coordinate, Height, below, {Max_x, Max_y, Forest}) ->
    count_visible_trees(Coordinate, Height, below, Max_y, 0, {Max_x, Max_y, Forest});

count_visible_trees(Coordinate, Height, left, Forest) ->
    count_visible_trees(Coordinate, Height, left, 0, 0, Forest);
count_visible_trees(Coordinate, Height, right, {Max_x, Max_y, Forest}) ->
    count_visible_trees(Coordinate, Height, right, Max_x, 0, {Max_x, Max_y, Forest}).


count_visible_trees({_X, Y}, _Height, above, Limit, Count, _Forrest) when Y < Limit ->
    Count;
count_visible_trees({X, Y} = Coordinate, Height, above, Limit, Count, Forrest) ->
    NeighborHeight = maps:get(Coordinate, element(3, Forrest)),
    if
        NeighborHeight < Height ->
            count_visible_trees({X, Y - 1}, Height, above, Limit, Count + 1, Forrest);
        true ->
            Count + 1
    end;

count_visible_trees({_X, Y}, _Height, below, Limit, Count, _Forrest) when Y > Limit ->
    Count;
count_visible_trees({X, Y} = Coordinate, Height, below, Limit, Count, Forrest) ->
    NeighborHeight = maps:get(Coordinate, element(3, Forrest)),
    if
        NeighborHeight < Height ->
            count_visible_trees({X, Y + 1}, Height, below, Limit, Count + 1, Forrest);
        true ->
            Count + 1
    end;

count_visible_trees({X, _Y}, _Height, right, Limit, Count, _Forrest) when X > Limit ->
    Count;
count_visible_trees({X, Y} = Coordinate, Height, right, Limit, Count, Forrest) ->
    NeighborHeight = maps:get(Coordinate, element(3, Forrest)),
    if
        NeighborHeight < Height ->
            count_visible_trees({X + 1, Y}, Height, right, Limit, Count + 1, Forrest);
        true ->
            Count + 1
    end;

count_visible_trees({X, _Y}, _Height, left, Limit, Count, _Forrest) when X < Limit ->
    Count;
count_visible_trees({X, Y} = Coordinate, Height, left, Limit, Count, Forrest) ->
    NeighborHeight = maps:get(Coordinate, element(3, Forrest)),
    if
        NeighborHeight < Height ->
            count_visible_trees({X - 1, Y}, Height, left, Limit, Count + 1, Forrest);
        true ->
            Count + 1
    end.



