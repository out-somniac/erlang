-module(rpn).
-import(math, [sqrt/1]).
-export([rpn/1]).

rpn(Expression) -> evaluate(string:tokens(Expression, " "), []).

add([A, B | Tail]) -> [A + B | Tail].
subtract([A, B | Tail]) -> [A - B | Tail].
multiply([A, B | Tail]) -> [A * B | Tail].
divide([A, B | Tail]) -> [A / B | Tail].
sqroot([A | Tail]) -> [sqrt(A) | Tail].

evaluate([], Stack) ->
    case length(Stack) of
        1 -> hd(Stack);
        _ -> throw(invalid_expression)
    end;

evaluate([Token | Tail], Stack) -> 
    case Token of
        "+" -> evaluate(Tail, add(Stack));
        "-" -> evaluate(Tail, subtract(Stack));
        "*" -> evaluate(Tail, multiply(Stack));
        "/" -> evaluate(Tail, divide(Stack));
        "sqrt" -> evaluate(Tail, sqroot(Stack));
         _  -> evaluate(Tail, [list_to_integer(Token) | Stack])
    end.

