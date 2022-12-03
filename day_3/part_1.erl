-module(part_1).
-export([run/0]).

run() ->
    {ok, Input} = file:read_file("input.txt"),

    Result = count_items(Input),
    io:fwrite("~p~n", [Result]).

count_items(Input) ->
    count_items(Input, {<<"">>, 0}, 0).

count_items(<<"\n">>, {Acc, Length}, Sum) ->
    {Compartment_1, Compartment_2} = split(Acc, trunc(Length / 2)),
    Priority = score(Compartment_1, Compartment_2),
    Sum + Priority;
count_items(<<Char:1/binary, Rest/binary>>, {Acc, Length}, Sum) when Char == <<"\n">> ->
    {Compartment_1, Compartment_2} = split(Acc, trunc(Length / 2)),
    Priority = score(Compartment_1, Compartment_2),
    count_items(Rest, {<<"">>, 0}, Sum + Priority);
count_items(<<Char:1/binary>>, {Acc, Length}, Sum) ->
    count_items(<<"\n">>, {<<Acc/binary, Char/binary>>, Length + 1}, Sum);
count_items(<<Char:1/binary, Rest/binary>>, {Acc, Length}, Sum) ->
    count_items(Rest, {<<Acc/binary, Char/binary>>, Length + 1}, Sum).

split(Binary, Size) ->
    {binary:part(Binary, {0, Size}), binary:part(Binary, {Size, Size})}.

score(Compartment_1, Compartment_2) ->
    Shared = sets:intersection(
               sets:from_list(binary:bin_to_list(Compartment_1)),
               sets:from_list(binary:bin_to_list(Compartment_2))
              ),
    SharedItem = sets:to_list(Shared),
    maps:get(SharedItem, priorities(), maps:get(string:lowercase(SharedItem), priorities()) + 26).

priorities() ->
    Alpha = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"],
    maps:from_list(lists:map(fun({Priority, Letter}) -> {Letter, Priority} end, lists:enumerate(1, Alpha))).
