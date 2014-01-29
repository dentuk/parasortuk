-module(quicksort).
-export([sort/3]).

-ifdef(debug).
-define(LOG(FMT, ARGS), io:format("[~p:~p] " FMT "~n", [?MODULE,?LINE|ARGS])).
-else.
-define(LOG(FMT, ARGS), true).
-endif.

generate_nodes([], _) -> [];
generate_nodes(L, DataSize) ->
    {H, T} = utils:split_at_rev(L, DataSize),
    Srv = self(),
    [spawn_link(fun() -> node_init(Srv, H) end) | generate_nodes(T, DataSize)].

nodes_start(Node0, Node1, Pivot) ->
    Node0 ! {self(), pivot, Pivot, inf, Node1},
    Node1 ! {self(), pivot, Pivot, sup, Node0}.

get_pivot([]) -> none;
get_pivot(Pairs) ->
    [{Node0,Node1}|T] = Pairs,
    ?LOG("~p: asking pivot to ~p", [self(), Node0]),
    Node0 ! {self(), head},
    receive
	{Node0, head, H0} -> H0;
	{Node0, no_head} ->
	    ?LOG("~p: asking pivot to ~p", [self(), Node1]),
	    Node1 ! {self(), head},
	    receive
		{Node1, head, H1} -> H1;
		{Node1, no_head} -> get_pivot(T)
	    end
    end.

process_pairs(Pairs) ->
    case get_pivot(Pairs) of
	none ->
	    ?LOG("~p: no pivot, continuing", [self()]),
	    ok;
	Pivot ->
	    ?LOG("~p: pivot is ~p", [self(), Pivot]),
	    lists:foreach(fun({Node0, Node1}) -> nodes_start(Node0, Node1, Pivot) end,
			  Pairs)
    end.

sort_step(Nodes, GroupSize) ->
    PairLists = utils:make_all_pairs(Nodes, GroupSize),
    lists:foreach(fun process_pairs/1, PairLists).

sort_all(_Nodes, 0) ->
    %[Node ! {self(), show} || Node <- Nodes],
    ok;
sort_all(Nodes, GroupSize) ->
    %[Node ! {self(), show} || Node <- Nodes],
    sort_step(Nodes, GroupSize),
    sort_all(Nodes, GroupSize div 2).

gather_data(Nodes) ->
    ?LOG("gathering data", []),
    [Node ! {self(), exit} || Node <- Nodes],
    gather_data_rec(Nodes).

gather_data_rec([]) -> [];
gather_data_rec([Node|Nodes]) ->
    ?LOG("~p: waiting for ~p", [self(), Node]),
    receive
	{Node, exit, L} -> [L|gather_data_rec(Nodes)]
    end.

sort(L, N, DataSize) ->
    ?LOG("~p: generating nodes", [self()]),
    process_flag(trap_exit, true),
    Nodes = generate_nodes(L, DataSize),
    ?LOG("~p: sorting", [self()]),
    sort_all(Nodes, (N div DataSize) div 2),
    gather_data(Nodes).

node_init(Srv, L) ->
    ?LOG("~p: created", [self()]),
    node_loop(Srv, L, []).

node_loop(Srv, L1, L2) ->
    ?LOG("~p: waiting for server ~p", [self(), Srv]),
    receive
	{Srv, head} ->
	    case L1 of
		[H|_] ->
		    Srv ! {self(), head, H};
		[] ->
		    case L2 of
			[] -> Srv ! {self(), no_head};
			[H|_] ->
			    Srv ! {self(), head, H}
		    end
	    end,
	    node_loop(Srv, L1, L2);
	{Srv, pivot, Pivot, KeepMe, Peer} ->
	    ?LOG("~p: pivot ~p peer ~p", [self(), Pivot, Peer]),
	    {Inf, Sup} = utils:split_by_rev(L1, L2, Pivot),
	    {MyData, PeerData} =
		case KeepMe of
		inf ->
		    Peer ! {self(), Srv, data, Sup},
		    ?LOG("~p: sent data to peer ~p, waiting", [self(), Peer]),
		    receive
                        {Peer, Srv, data, Other} ->
			    ?LOG("~p: received data from peer ~p", [self(), Peer]),
			    {Inf, Other}
		    end;
		_ ->
		    ?LOG("~p: waiting data from peer ~p", [self(), Peer]),
		    receive
		        {Peer, Srv, data, Other} ->
			    ?LOG("~p: received data from peer ~p, sending back", [self(), Peer]),
			    Peer ! {self(), Srv, data, Inf},
			    {Sup, Other}
                    end
	    end,
	    node_loop(Srv, MyData, PeerData);
	{Srv, show} ->
	    io:format("~p: ~p~n", [self(), {L1, L2}]),
	    node_loop(Srv, L1, L2);
	{Srv, exit} ->
	    ?LOG("~p: terminating", [self()]),
	    Srv ! {self(), exit, lists:sort(L1 ++ L2)}
    end.
