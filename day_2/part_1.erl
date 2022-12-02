-module(part_1).
-export([run/0]).

run() ->
    {ok, Input} = file:read_file("input.txt"),
    Matches = binary:split(Input, <<"\n">>, [global]),
    Sum = lists:sum(lists:map(fun(Match) -> score_match(Match) end, Matches)),
    io:fwrite("~p~n", [Sum]).

%% A for Rock, B for Paper, and C for Scissors
%% X for Rock, Y for Paper, and Z for Scissors
%% (1 for Rock, 2 for Paper, and 3 for Scissors)
%% plus the score for the outcome of the round
%% (0 if you lost, 3 if the round was a draw, and 6 if you won)
score_match([<<"A">>, You]) ->
    case You of
        <<"X">> -> 3 + 1;
        <<"Y">> -> 6 + 2;
        <<"Z">> -> 0 + 3
    end;
score_match([<<"B">>, You]) ->
    case You of
        <<"X">> -> 0 + 1;
        <<"Y">> -> 3 + 2;
        <<"Z">> -> 6 + 3
    end;
score_match([<<"C">>, You]) ->
    case You of
        <<"X">> -> 6 + 1;
        <<"Y">> -> 0 + 2;
        <<"Z">> -> 3 + 3
    end;
score_match([]) ->
    0;
score_match(Match) ->
    score_match(binary:split(Match, <<" ">>, [trim])).
