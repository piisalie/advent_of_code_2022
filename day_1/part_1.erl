-module(part_1).
-export([run/0]).

run() ->
    {ok, Input} = file:read_file("input.txt"),
    Elfs = binary:split(Input, <<"\n\n">>, [global]),
    Max = lists:max(lists:map(fun(Elf) -> count_calories(Elf, 0) end, Elfs)),
    io:fwrite("~p~n", [Max]).

count_calories([], Sum) ->
    Sum;
count_calories([Calories | Rest], Sum) ->
    count_calories(Rest, Sum + binary_to_integer(Calories));
count_calories(Calories, Sum) ->
    count_calories(binary:split(Calories, <<"\n">>, [global, trim]), Sum).
