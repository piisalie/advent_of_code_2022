-module(part_2).
-export([run/0]).

run() ->
    {ok, Input} = file:read_file("input.txt"),
    Matches = binary:split(Input, <<"\n">>, [global]),
    Sum = lists:sum(lists:map(fun(Match) -> score_match(Match) end, Matches)),
    io:fwrite("~p~n", [Sum]).

%% A for Rock, B for Paper, and C for Scissors
%% X means you need to lose
%% Y means you need to end the round in a draw
%% and Z means you need to win
%% (1 for Rock, 2 for Paper, and 3 for Scissors)
%% plus the score for the outcome of the round
%% (0 if you lost, 3 if the round was a draw, and 6 if you won)
score_match([<<"A">>, You]) ->
    case You of
        <<"X">> -> 0 + 3;
        <<"Y">> -> 3 + 1;
        <<"Z">> -> 6 + 2
    end;
score_match([<<"B">>, You]) ->
    case You of
        <<"X">> -> 0 + 1;
        <<"Y">> -> 3 + 2;
        <<"Z">> -> 6 + 3
    end;
score_match([<<"C">>, You]) ->
    case You of
        <<"X">> -> 0 + 2;
        <<"Y">> -> 3 + 3;
        <<"Z">> -> 6 + 1
    end;
score_match([]) ->
    0;
score_match(Match) ->
    score_match(binary:split(Match, <<" ">>, [trim])).
