-module(part_2).
-export([run/0]).

run() ->
    {ok, Input} = file:read_file("input.txt"),
    Split = binary:split(Input, <<"\n">>, [global]),
    Result = count_overlaps(Split),
    io:fwrite("~p~n", [Result]).

count_overlaps(Sections) ->
    count_overlaps(Sections, 0).

count_overlaps([<<>>], Count) -> Count;
count_overlaps([Sections | Rest], Count) ->
    [First, Second] = binary:split(Sections, <<",">>),
    [Start1, End1] = binary:split(First, <<"-">>),
    [Start2, End2] = binary:split(Second, <<"-">>),
    ToAdd = is_contained(
              binary_to_integer(Start1),
              binary_to_integer(Start2),
              binary_to_integer(End1),
              binary_to_integer(End2)),
    count_overlaps(Rest, Count + ToAdd).

is_contained(Start1, Start2, End1, _End2) when (Start2 =< End1) and (Start2 >= Start1)->
    1;
is_contained(Start1, _Start2, End1, End2) when (End2 >= Start1) and (End2 =< End1) ->
    1;
is_contained(Start1, Start2, End1, End2) when (Start1 >= Start2) and (End1 =< End2) ->
    1;
is_contained(Start1, Start2, End1, End2) when (Start2 >= Start1) and (End2 =< End1) ->
    1;
is_contained(_, _, _, _) ->
    0.



