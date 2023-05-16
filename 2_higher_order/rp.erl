-module(rp).
-export([replace/1]).

replace_1(String) -> lists:map(fun ($e) -> $o; ($a) -> $e end, String).
replace_2(List) ->  length(lists:filter(fun(X) -> X podzielny przez 3, List)).