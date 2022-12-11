-module(part_1).
-export([run/1]).
-include_lib("eunit/include/eunit.hrl").

%% build a coordinate plane
%% traverse each point
%% count if every point in NWSE is less than this height
%%
%% optimizations: exit early if possible for a given point
%%                can exclude all outside points

run_test() -> 21 = run("test_input.txt").

run(InputFile) ->
    {ok, Input} = file:read_file(InputFile),
    {Max_x, Max_y, Forrest} = parse(Input),

    maps:fold(
      fun(Coordinates, _Height, Acc) ->
              Visible = is_visible(Coordinates, Max_x, Max_y, Forrest),
              if
                  Visible -> Acc + 1;
                  true -> Acc
              end
      end,
      0,
      Forrest
     ).

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

is_visible_test() ->
    {ok, Input} = file:read_file("test_input.txt"),
    {_, _, Forrest} = parse(Input),
    true = is_visible({1,1}, 4, 4, Forrest),
    true = is_visible({1,2}, 4, 4, Forrest),
    false = is_visible({1,3}, 4, 4, Forrest).

is_visible({X, _Y}, Max_x, _Max_y, _Forrest) when (X == Max_x) or (X == 0) ->
    true;
is_visible({_X, Y}, _Max_x, Max_y, _Forrest) when (Y == Max_y) or (Y == 0) ->
    true;
is_visible({X, Y} = Coordinate, Max_x, Max_y, Forrest) ->
    Height = maps:get(Coordinate, Forrest),

    Above = lists:max(maps:values(maps:with(coords({X, Y - 1}, above, 0), Forrest))),
    Below = lists:max(maps:values(maps:with(coords({X, Y + 1}, below, Max_y), Forrest))),
    Left = lists:max(maps:values(maps:with(coords({X - 1, Y}, left, 0), Forrest))),
    Right = lists:max(maps:values(maps:with(coords({X + 1, Y}, right, Max_x), Forrest))),

    (Height > Above) or (Height > Below) or (Height > Left) or (Height > Right).

coords(Coordinate, Direction, Limit) ->
    coords(Coordinate, Direction, Limit, []).

coords({X, _Y}, left, Limit, Acc) when X < Limit ->
    Acc;
coords({X, Y}, left, Limit, Acc) ->
    coords({X - 1, Y}, left, Limit, [{X, Y} | Acc]);

coords({X, _Y}, right, Limit, Acc) when X > Limit ->
    Acc;
coords({X, Y}, right, Limit, Acc) ->
    coords({X + 1, Y}, right, Limit, [{X, Y} | Acc]);

coords({_X, Y}, above, Limit, Acc) when Y < Limit ->
    Acc;
coords({X, Y}, above, Limit, Acc) ->
    coords({X, Y - 1}, above, Limit, [{X, Y} | Acc]);

coords({_X, Y}, below, Limit, Acc) when Y > Limit ->
    Acc;
coords({X, Y}, below, Limit, Acc) ->
    coords({X, Y + 1}, below, Limit, [{X, Y} | Acc]).

