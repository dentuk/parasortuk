-module(comparator).
-export([compare/2, compare_many/3]).

compare(N, M) ->
    L = utils:generate_data(N),
    {T1, _} = timer:tc(lists, sort, [L]),
    {T2, _} = timer:tc(quicksort, sort, [L, N, M]),
    [T1, T2].

compare_many(Many, N, M) ->
    [S / Many || S <- sum_compare_many(Many, N, M)].

sum_compare_many(1, N, M) -> compare(N, M);
sum_compare_many(Many, N, M) ->
    sum(compare(N, M), sum_compare_many(Many - 1, N, M)). 

sum([], _) -> [];
sum(_, []) -> [];
sum([H1|T1], [H2|T2]) -> [H1 + H2|sum(T1, T2)].
