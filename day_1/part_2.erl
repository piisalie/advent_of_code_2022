-module(part_2).
-export([run/0]).

run() ->
    {ok, Input} = file:read_file("input.txt"),
    Elfs = binary:split(Input, <<"\n\n">>, [global]),
    [First, Second, Third | _rest] = lists:reverse(lists:sort(lists:map(fun(Elf) -> count_calories(Elf, 0) end, Elfs))),
    io:fwrite("~p~n", [First + Second + Third]).

count_calories([], Sum) ->
    Sum;
count_calories([Calories | Rest], Sum) ->
    count_calories(Rest, Sum + binary_to_integer(Calories));
count_calories(Calories, Sum) ->
    count_calories(binary:split(Calories, <<"\n">>, [global, trim]), Sum).
