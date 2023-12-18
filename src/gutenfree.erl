-module(gutenfree).
-export([generate_chain/2, traverse/2, traverse/3]).

-include_lib("stdlib/include/rand.hrl").

%% Public API

%% Generates a markov chain from a binary using `order` sized terms
-spec generate_chain(binary(), integer()) -> {ok, term()}.
generate_chain(Bin, Order) ->
    {ok, gutenfree_table:generate(Bin, Order)}.

%% Returns the resulting string from traversing a markov chain `n` times
-spec traverse(term(), integer()) -> {ok, binary()} | {error, atom()}.
traverse(Chain, N) ->
    traverse(Chain, N, fun random/1).

%% Returns the resulting string from traversing a markov chain `n` times
-spec traverse(term(), integer(), fun()) -> {ok, binary()} | {error, atom()}.
traverse(Chain, N, Method) when is_function(Method, 1) ->
    do_traverse(Chain, N, Method);
traverse(Chain, N, default) ->
    do_traverse(Chain, N, fun random/1);
traverse(_, _, _) ->
    {error, bad_method}.

%% Helpers

random(Edges) ->
    X = rand:uniform(),
    {Edge, _} = lists:min([{{K, V}, V - X} || {K, V} <- Edges]),
    Edge.

do_traverse(Chain, 0, _, _, Acc) ->
    {ok, Acc};
do_traverse(Chain, N, Method, Seq, Acc) ->
    case maps:get(Seq, Chain, undefined) of
        undefined -> {ok, Acc};
        Edges ->
            Term = Method(Edges),
            NSeq = lists:sublist(Seq, 2) ++ [Term],
            do_traverse(Chain, N - 1, Method, NSeq, lists:flatten([Acc, " ", Term]))
    end.

do_traverse(Chain, N, Method) ->
    StartSeq = random_element(maps:keys(Chain)),
    do_traverse(Chain, N, Method, StartSeq, string:join(StartSeq, " ")).

random_element(List) ->
    lists:nth(rand:uniform(length(List)), List).
