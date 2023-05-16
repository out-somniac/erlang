-module(qs).
-export([quicksort/1, random_elements/3, compare_speeds/3, compare/0]).

% Test generation
random_elements(Total, Min, Max) ->
    [rand:uniform(Max) + Min - 1 || _ <- lists:seq(1, Total)].


% Algorithm
less_than(List, Arg) -> lists:filter(fun(X) -> X > Arg end, List).
greater_than(List, Arg) -> lists:filter(fun(X) -> X < Arg end, List).

quicksort([]) -> [];
quicksort(List) -> 
    Pivot = lists:nth(rand:uniform(length(List)), List),
    quicksort(greater_than(List, Pivot)) ++ [Pivot] ++ quicksort(less_than(List, Pivot)).

% Benchmarking
compare_speeds(List, Custom, Library) -> 
    {CustomTime, _} = timer:tc(Custom, [List]),
    {LibraryTime, _} = timer:tc(Library, [List]),
    io:format("Custom ~.3f Library ~.3f", [CustomTime/1000000, LibraryTime/1000000]).

compare() -> compare_speeds(random_elements(100000000, 1, 100),fun quicksort/1, fun lists:sort/1).