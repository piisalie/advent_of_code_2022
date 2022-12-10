-module(part_2).
-export([run/1]).

run(InputFile) ->
    {ok, Input} = file:read_file(InputFile),
    FileSystem = traverse_the_tree(Input),
    TotalDirectorySize = maps:get([<<"/">>], FileSystem),
    SpaceNeeded = 30000000 - (70000000 - TotalDirectorySize),
    Candidates = maps:filtermap(fun(_DirName, Size) -> Size >= SpaceNeeded end, FileSystem),

    SmallestViableDir = lists:min(maps:values(Candidates)),
    io:fwrite("~p~n", [SmallestViableDir]).

traverse_the_tree(Input) ->
    traverse_the_tree(binary:split(Input, <<"\n">>, [global]), [], #{[<<"/">>] => 0}).

traverse_the_tree([], _, FileSystem) ->
    FileSystem;
traverse_the_tree([<<"$", " cd ", "..">> | Rest], [_LastDir | CurrentPath], FileSystem) ->
    traverse_the_tree(Rest, CurrentPath, FileSystem);
traverse_the_tree([<<"$", " cd ", Path/binary>> | Rest], CurrentPath, FileSystem) ->
    NewPath = [Path | CurrentPath],
    NewFileSystem = maps:put(NewPath, 0, FileSystem),
    traverse_the_tree(Rest, NewPath, NewFileSystem);
traverse_the_tree([<<"$", " ls">> | Rest], Path, FileSystem) ->
    {{_Directories, Files}, NextCommands} = find_next_command(Rest),
    CurrentDirectorySize = lists:foldl(fun({_Name, Size}, Sum) ->
                                               Sum + binary_to_integer(Size)
                                       end, 0, Files),
    NewFileSystem = add_to_parent_dirs(Path, CurrentDirectorySize, FileSystem),
    traverse_the_tree(NextCommands, Path, NewFileSystem).

find_next_command(Lines) ->
    find_next_command(Lines, {[], []}).

find_next_command([<<>>], DirContents) ->
    {DirContents, []};
find_next_command([<<"$", _/binary>> | _Rest] = RemainingCommands, DirContents) ->
    {DirContents, RemainingCommands};
find_next_command([<<"dir ", DirName/binary>> | Rest], {Directories, Files}) ->
    find_next_command(Rest, {[DirName | Directories], Files});
find_next_command([File | Rest], {Directories, Files}) ->
    [Size, FileName] = binary:split(File, <<" ">>),
    find_next_command(Rest, {Directories, [{FileName, Size} | Files]}).

add_to_parent_dirs([], _, FileSystem) ->
    FileSystem;
add_to_parent_dirs([_Hd | Rest] = CurrentPath, Size, FileSystem) ->
    OldSize = maps:get(CurrentPath, FileSystem),
    UpdatedFileSystem = maps:update(CurrentPath, OldSize + Size, FileSystem),
    add_to_parent_dirs(Rest, Size, UpdatedFileSystem).

