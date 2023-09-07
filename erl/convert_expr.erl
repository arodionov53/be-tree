% 
% Convers files with raw ids and boolean expression ito two files: 
% For example
% convert_expr:convert_files("id-const-expr.txt", "../build/tests/data/betree_exprs", "../build/tests/data/betree_constants").
% 
% Before running check that ther is no \n or \n\n in id-const-expr.txt
% 

-module(convert_expr).

-export([convert_files/0, convert_files/3]).


-spec convert_files() -> any().
convert_files() -> convert_files("id-const-expr.txt", "../build/tests/data/betree_exprs", "../build/tests/data/betree_constants").

-spec convert_files(string(), string(), string()) -> any().
convert_files(FileFrom, ExprTo, ConstTo) ->
    case file:consult(FileFrom) of
        {ok, [First | Tail]} -> 
            {Ids, Bin} = separate(First),
            write_both({Ids, Bin}, ExprTo, ConstTo, write),
            lists:foreach(fun(X) -> write_both(separate(X), ExprTo, ConstTo, append) end, Tail);
        Else -> Else
    end.

-spec separate(tuple()) -> {[integer()], binary()}.
separate({_, Ids, Binary}) -> 
    {[N || {_, N} <- Ids], Binary}.

-spec write_ids([integer()], string(), write | append) -> any().
write_ids(Ids, FileName, Option) -> 
    file:write_file(FileName, io_lib:fwrite("~p,~p,~p,~p~n", Ids), [Option]).

-spec write_bin(binary(), string(), write | append) -> any().
write_bin(Bin, FileName, Option) -> 
    file:write_file(FileName, Bin, [Option]),
    file:write_file(FileName, "\n", [append]).

-spec write_both(tuple(), string(), string(), write | append) -> any(). 
write_both({Ids, Bin}, ExprTo, ConstTo, Option) ->
    write_bin(Bin, ExprTo, Option),
    write_ids(Ids, ConstTo, Option).
