-module(part_2).
-export([run/0]).

run() ->
    {ok, Input} = file:read_file("input.txt"),
    Split = binary:split(Input, <<"\n">>, [global]),
    Result = find_badges(Split),
    io:fwrite("~p~n", [Result]).

find_badges(Elfs) ->
    find_badges(Elfs, 0).

find_badges([<<>>], Total) -> Total;
find_badges([Elf1, Elf2, Elf3 | Rest], Total) ->
    SharedG1 = sets:intersection(
               sets:from_list(binary:bin_to_list(Elf1)),
               sets:from_list(binary:bin_to_list(Elf2))
              ),
    SharedG2 = sets:intersection(
               sets:from_list(binary:bin_to_list(Elf2)),
               sets:from_list(binary:bin_to_list(Elf3))
              ),
    Shared = sets:intersection(
               SharedG1,
               SharedG2
              ),
    find_badges(Rest, score(Shared) + Total).

score(Shared) ->
    SharedItem = sets:to_list(Shared),
    maps:get(SharedItem, priorities(), maps:get(string:lowercase(SharedItem), priorities()) + 26).

priorities() ->
    Alpha = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"],
    maps:from_list(lists:map(fun({Priority, Letter}) -> {Letter, Priority} end, lists:enumerate(1, Alpha))).
