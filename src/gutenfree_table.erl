-module(gutenfree_table).
-export([generate/2]).

generate(Bin, Order) -> 
    init(Bin, Order, [], <<>>).

init(<<$\s, Rest/bits>>, 0, Acc, _) ->
    build(<<>>, {dict:new(), Acc}, <<>>);
init(<<$\s, Rest/bits>>, Order, Acc, Term) ->
    init(trim(Rest), Order - 1, [Term | Acc], <<>>);
init(<<$\t, Rest/bits>>, Order, Acc, Term) ->
    init(trim(Rest), Order - 1, [Term | Acc], <<>>);
init(<<$\r, Rest/bits>>, Order, Acc, Term) ->
    init(trim(Rest), Order, Acc, Term);
init(<<$\n, Rest/bits>>, Order, Acc, <<>>) ->
    init(trim(Rest), Order, Acc, <<>>);
init(<<$\n, Rest/bits>>, Order, Acc, Term) ->
    init(trim(Rest), Order, Acc, <<Term/binary, " ">>);
init(<<Char, Rest/bits>>, Order, Acc, Term) ->
    init(Rest, Order, Acc, <<Term/binary, Char>>).

build(<<>>, {Table, _}, <<>>) ->
    normalize(Table);
build(<<>>, S, Term) ->
    {Table, _} = update(S, Term),
    normalize(Table);
build(<<$\s, Rest/bits>>, S, Term) ->
    build(trim(Rest), update(S, Term), <<>>);
build(<<$\t, Rest/bits>>, S, Term) ->
    build(trim(Rest), update(S, Term), <<>>);
build(<<$\r, Rest/bits>>, S, Term) ->
    build(trim(Rest), S, Term);
build(<<$\n, Rest/bits>>, S, <<>>) ->
    build(trim(Rest), S, <<>>);
build(<<$\n, Rest/bits>>, S, Term) ->
    build(trim(Rest), S, <<Term/binary, " ">>);
build(<<Char, Rest/bits>>, S, Term) ->
    build(Rest, S, <<Term/binary, Char>>).

update({Table, Seq}, Term) ->
    Table1 = 
        case dict:find(lists:reverse(Seq), Table) of
            {ok, TMap} ->
                TMap1 = case dict:find(Term, TMap) of
                    {ok, Val} -> dict:store(Term, Val + 1, TMap);
                    error -> dict:store(Term, 1, TMap)
                end,
                dict:store(lists:reverse(Seq), TMap1, Table);
            error ->
                dict:store(lists:reverse(Seq), dict:store(Term, 1, dict:new()), Table)
        end,
    {Table1, [Term | lists:sublist(Seq, length(Seq) - 1)]}.

normalize(Table) ->
    dict:fold(fun(Node, Transitions, Acc) ->
        Sum = lists:sum(dict:values(Transitions)),
        NewTrans = dict:fold(fun(Transition, Val, TAcc) ->
            dict:store(Transition, Val / Sum, TAcc)
        end, dict:new(), Transitions),
        dict:store(Node, NewTrans, Acc)
    end, dict:new(), Table).

trim(<<$\s, Rest/bits>>) -> trim(Rest);
trim(<<$\t, Rest/bits>>) -> trim(Rest);
trim(Bin) -> Bin.
