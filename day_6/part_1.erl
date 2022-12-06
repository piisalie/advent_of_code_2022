-module(part_1).
-export([run/0]).

run() ->
    {ok, Input} = file:read_file("input.txt"),

    Result = find_non_repeats(Input),
    io:fwrite("~p~n", [Result]).

find_non_repeats(Input) ->
    find_non_repeats(Input, {[], 0}).

find_non_repeats(<<Char:1/binary, Rest/binary>>, {Seen, Idx}) ->
    case is_unique([Char | Seen]) of
        {true, _} ->
            Idx + 1;
        {false, New} ->
            find_non_repeats(Rest, {lists:sublist(New,3), Idx + 1})
    end.

is_unique(List) when length(List) < 4 ->
    {false, List};
is_unique(List) ->
     {length(List) == length(lists:uniq(List)), List}.
