parasortuk
==========

A parallel quicksort implementation using Erlang

Modules
-------

* `utils`: mainly list manipulation functions, used by other modules ;
* `quicksort`: parallel sorting functions ;
* `comparator`: tools for benchmarking.

Howto: Sorting a List
---------------------

The list length N must be a power of two for the sorting algorithm to
work. The list is first split into N/P parts of length P, and there
will then be N/P processes working together through several steps in
order to sort the global list. Each process will work on a different
part of the global list.

The result is a list of lists. Since each sublist is the one that was
finally returned by a given process, the sublists' sizes homogeneity
allows to know if the sort was fairly distributed. You can then call
`lists:flatten/1` in order to get a flattened version of the list.

```erlang
dentuk@tank ~/git/parasortuk $ erl
Eshell V5.10.4  (abort with ^G)
1> c(utils).
{ok,utils}
2> c(quicksort).
{ok,quicksort}
3> N = 16, L0 = utils:generate_data(N).
[0.4435846174457203,0.7230402056221108,0.94581636451987,
 0.5014907142064751,0.311326754804393,0.597447524783298,
 0.915656206971831,0.6669572934854013,0.47712105608919275,
 0.5965100813402789,0.14210821770124227,0.20944855618709624,
 0.6971407843005519,0.15981142006315596,0.5582558083752902,
 0.21497304823532137]
4> P = 4, L1 = quicksort:sort(L0, N, P).
[[0.14210821770124227,0.15981142006315596,
  0.20944855618709624,0.21497304823532137,0.311326754804393,
  0.4435846174457203],
 [0.47712105608919275,0.5014907142064751],
 [0.5582558083752902,0.5965100813402789],
 [0.597447524783298,0.6669572934854013,0.6971407843005519,
  0.7230402056221108,0.915656206971831,0.94581636451987]]
5> L2 = lists:flatten(L1).
[0.14210821770124227,0.15981142006315596,
 0.20944855618709624,0.21497304823532137,0.311326754804393,
 0.4435846174457203,0.47712105608919275,0.5014907142064751,
 0.5582558083752902,0.5965100813402789,0.597447524783298,
 0.6669572934854013,0.6971407843005519,0.7230402056221108,
 0.915656206971831,0.94581636451987]
```

Howto: Enabling Debug Mode
--------------------------

In order to enable debug mode, define the `debug` macro 
when compiling the `quicksort` module, for instance:
`c(quicksort, {d, debug}).`.


