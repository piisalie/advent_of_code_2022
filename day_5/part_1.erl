-module(part_1).
-export([run/0]).

run() ->
    {ok, Input} = file:read_file("input.txt"),
    [UnparsedStacks, Procedure] = binary:split(Input, <<"\n\n">>),
    Stacks = parse_stacks(binary:split(UnparsedStacks, <<"\n">>, [global])),
    Result = handle_procedure(binary:split(Procedure, <<"\n">>, [global]), Stacks),
    lists:foldr(fun({_Idx, Stack}, Acc) ->
                         End = hd(Stack),
                         [End | Acc]
                 end, [], Result).

parse_stacks(Input) ->
    parse_stacks(Input, []).

parse_stacks(Input, _Stacks) ->
    [IndexRow | StackData] = lists:reverse(Input),
    Index = build_index(IndexRow),
    build_stack_lists(StackData, Index).

build_index(IndexRow) ->
    build_index(IndexRow, [], 0).

build_index(<<>>, Acc, _Idx) ->
    lists:reverse(Acc);
build_index(<<" ", Rest/binary>>, Acc, Idx) ->
    build_index(Rest, Acc, Idx +1);
build_index(<<StackNumber:1/binary, Rest/binary>>, Acc, Idx) ->
    build_index(Rest, [{StackNumber, Idx} | Acc], Idx + 1).

build_stack_lists(Stacks, StackIndex) ->
    build_stack_lists(Stacks, StackIndex, maps:new()).

build_stack_lists([], _StackIndex, Acc) ->
    Acc;
build_stack_lists([Row | Rest], StackIndex, Acc) ->
    ItemsInThisRow = lists:filtermap(fun({StackNumber, Idx}) ->
                                       case binary:part(Row, Idx, 1) of
                                           <<" ">> -> false;
                                           <<Item>> -> {true, {StackNumber, Item}}
                                       end
                               end, StackIndex),
    NewAcc = lists:foldl(fun({StackNumber, Item}, StackLists) ->
                                 maps:update_with(StackNumber, fun(Value) ->
                                                                       [Item | Value]
                                                               end, [Item], StackLists)
                         end, Acc, ItemsInThisRow),
    build_stack_lists(Rest, StackIndex, NewAcc).

handle_procedure([<<>>], Stacks) ->
    lists:sort(maps:to_list(Stacks));
handle_procedure([Instruction | Rest], Stacks) ->
    [_, Count, _, Start, _, Dest] = binary:split(Instruction, <<" ">>, [global]),

    StartStack = maps:get(Start, Stacks),
    DestStack = maps:get(Dest, Stacks),
    {ToMove, LeftOver} = lists:split(binary_to_integer(Count), StartStack),
    Changes = #{Start => LeftOver, Dest => lists:reverse(ToMove) ++ DestStack},
    NewStacks = maps:merge(Stacks, Changes),

    handle_procedure(Rest, NewStacks).
