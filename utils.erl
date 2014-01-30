-module(utils).
-export([generate_data/1,
	 split_at/2, split_at_rev/2,
	 split_by/2, split_by_rev/2, split_by/3, split_by_rev/3,
	 make_pairs/2, make_all_pairs/2]).

% --- Random data generation ---

% Generate a list of N pseudo-random numbers uniformly distributed
% between 0 and 1.
generate_data(0) -> [];
generate_data(N) -> [random:uniform() | generate_data(N-1)].


% --- Splitting a list at a given position ---

% Get the N first elements of L on one hand, the other elements on the
% other hand.
split_at(L, N) ->
    {HRev, T} = split_at_rev(L, N),
    {lists:reverse(HRev), T}.

% Get the N first elements of L on one hand (in reverse order), the
% other elements on the other hand.
split_at_rev(L, N) -> split_at_rev(L, N, []).

% Auxiliary function for split_at_rev/2.
split_at_rev([], _, Stack) -> {Stack, []};
split_at_rev(L, 0, Stack) -> {Stack, L};
split_at_rev([H|T], N, Stack) -> split_at_rev(T, N - 1, [H|Stack]).


% --- Partitionning a list over a pivot ---

% Partition L into two sublists : one containing elements <= P,
% the other containing elements > P.
split_by(L, P) ->
    {InfRev, SupRev} = split_by_rev(L, P),
    {lists:reverse(InfRev), lists:reverse(SupRev)}.

% Partition L into two sublists : one containing elements <= P,
% the other containing elements > P. The elements will be in reversed
% order.
split_by_rev(L, P) -> split_by_rev(L, P, [], []).

% Auxiliary function for split_by_rev/2.
split_by_rev([], _, Inf, Sup) -> {Inf, Sup};
split_by_rev([H|T], P, Inf, Sup) when H > P -> split_by_rev(T, P, Inf, [H|Sup]) ;
split_by_rev([H|T], P, Inf, Sup) -> split_by_rev(T, P, [H|Inf], Sup).

% Same as split_by(L1 ++ L2, P) but faster.
split_by(L1, L2, P) ->
    {InfRev, SupRev} = split_by_rev(L1, L2, P),
    {lists:reverse(InfRev), lists:reverse(SupRev)}.

% Same as split_by_rev(L1 ++ L2, P) but faster.
split_by_rev(L1, L2, P) ->
    {InfRev, SupRev} = split_by_rev(L1, P, [], []),
    split_by_rev(L2, P, InfRev, SupRev).

% --- Splitting a list into groups of paired elements ---

% Make GroupSize pairs of objects on one hand, and get the remaining
% elements on the other hand.
% For instance:
%     1> utils:make_pairs([1,2,3,4,5,6,7,8],2).
%     {[{1,4},{2,3}],[5,6,7,8]}
make_pairs(L, GroupSize) ->
    {HRev, T} = split_at_rev(L, GroupSize),
    make_pairs(HRev, T, []).

% Auxiliary function for make_pairs/2.
make_pairs([], T, Pairs) -> {Pairs, T};
make_pairs(T, [], Pairs) -> {Pairs, T};
make_pairs([H0|T0], [H1|T1], Pairs) -> make_pairs(T0, T1, [{H0, H1}|Pairs]).

% Apply make_pairs/2 until all list elements are paired, returning a
% list of paired-elements lists.
% Example:
%     4> utils:make_all_pairs([1,2,3,4,5,6,7,8],2).
%     [[{1,4},{2,3}],[{5,8},{6,7}]]
make_all_pairs([], _) -> [];
make_all_pairs(L, GroupSize) ->
    {Pairs, T} = make_pairs(L, GroupSize),
    case Pairs of
	[] -> [];
	_ -> [Pairs|make_all_pairs(T, GroupSize)]
    end.

