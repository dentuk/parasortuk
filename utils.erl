-module(utils).
-export([generate_data/1,
	 split_at/2, split_at_rev/2,
	 split_by/2, split_by_rev/2, split_by/3, split_by_rev/3,
	 make_pairs/2, make_all_pairs/2]).

% --- Génération de nombres aléatoires ---

% Générer une liste de N nombres aléatoires compris entre 0 et 1.
generate_data(0) -> [];
generate_data(N) -> [random:uniform() | generate_data(N-1)].


% --- Séparation des N premiers éléments d'une liste ---

% Récupérer les N premiers éléments de la liste L d'une part, les
% éléments restants d'autre part.
split_at(L, N) ->
    {HRev, T} = split_at_rev(L, N),
    {lists:reverse(HRev), T}.

% Récupérer les N premiers éléments (en sens inverse) de la liste
% L d'une part, les éléments restants d'autre part.
split_at_rev(L, N) -> split_at_rev(L, N, []).

% Fonction auxiliare de split_at_rev/2.
split_at_rev([], _, Stack) -> {Stack, []};
split_at_rev(L, 0, Stack) -> {Stack, L};
split_at_rev([H|T], N, Stack) -> split_at_rev(T, N - 1, [H|Stack]).


% --- Séparation d'une liste selon un pivot ---

% Séparer la liste L en deux sous-listes : celle des éléments <= P,
% et celle des éléments > P.
split_by(L, P) ->
    {InfRev, SupRev} = split_by_rev(L, P),
    {lists:reverse(InfRev), lists:reverse(SupRev)}.

% Séparer la liste L en deux sous-listes : celle des éléments <= P,
% et celle des éléments > P.
split_by_rev(L, P) -> split_by_rev(L, P, [], []).

split_by_rev([], _, Inf, Sup) -> {Inf, Sup};
split_by_rev([H|T], P, Inf, Sup) when H > P -> split_by_rev(T, P, Inf, [H|Sup]) ;
split_by_rev([H|T], P, Inf, Sup) -> split_by_rev(T, P, [H|Inf], Sup).

split_by(L1, L2, P) ->
    {InfRev, SupRev} = split_by_rev(L1, L2, P),
    {lists:reverse(InfRev), lists:reverse(SupRev)}.

split_by_rev(L1, L2, P) ->
    {InfRev, SupRev} = split_by_rev(L1, P, [], []),
    split_by_rev(L2, P, InfRev, SupRev).

make_pairs(L, GroupSize) ->
    %io:format("~p: make_pairs(~p,~p)~n",[self(),L, GroupSize]),
    {HRev, T} = split_at_rev(L, GroupSize),
    make_pairs(HRev, T, []).

make_pairs([], T, Pairs) -> {Pairs, T};
make_pairs(T, [], Pairs) -> {Pairs, T};
make_pairs([H0|T0], [H1|T1], Pairs) -> make_pairs(T0, T1, [{H0, H1}|Pairs]).


make_all_pairs([], _) -> [];
make_all_pairs(L, GroupSize) ->
    %io:format("~p: make_all_pairs(~p,~p)~n",[self(),L, GroupSize]),
    {Pairs, T} = make_pairs(L, GroupSize),
    case Pairs of
	[] -> [];
	_ -> [Pairs|make_all_pairs(T, GroupSize)]
    end.

