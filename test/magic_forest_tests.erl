%%%-------------------------------------------------------------------
%%% @author vavaka
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(magic_forest_tests).

-include("magic_forest.hrl").
-include_lib("eunit/include/eunit.hrl").

%% ---------------------------------------------------------------
%% Test functions
%% ---------------------------------------------------------------

new_test() ->
  %% should initialize forest of exact size
  ?assertEqual(0, orddict:size(magic_forest:new({0, []}))),
  ?assertEqual(5, orddict:size(magic_forest:new({5, []}))),

  %% should correctly initialize node without edges
  Forest1 = magic_forest:new({1, []}),
  assert_node({0, [], []}, magic_forest:node(0, Forest1)),

  %% should correctly initialize nodes with edges
  Forest2 = magic_forest:new({8, [
    {1, 2},
    {2, 3},
    {2, 4},
    {5, 7},
    {6, 7}
  ]}),
  assert_node({0, [], []}, magic_forest:node(0, Forest2)),
  assert_node({1, [], [2]}, magic_forest:node(1, Forest2)),
  assert_node({2, [1], [3, 4]}, magic_forest:node(2, Forest2)),
  assert_node({3, [2], []}, magic_forest:node(3, Forest2)),
  assert_node({4, [2], []}, magic_forest:node(4, Forest2)),
  assert_node({5, [], [7]}, magic_forest:node(5, Forest2)),
  assert_node({6, [], [7]}, magic_forest:node(6, Forest2)),
  assert_node({7, [5, 6], []}, magic_forest:node(7, Forest2)).

node_test() ->
  Forest = magic_forest:new({2, []}),

  %% should return node if valid id
  assert_node({0, [], []}, magic_forest:node(0, Forest)),
  assert_node({1, [], []}, magic_forest:node(1, Forest)),

  %% should raise error if invalid id
  ?assertException(error, function_clause, magic_forest:node(2, Forest)).

trees_test() ->
  FindTreeIds = fun(ForestSpec) ->
    Forest = magic_forest:new(ForestSpec),
    Trees = magic_forest:trees(Forest),

    TreeIds = lists:map(fun(#node{id = NodeId}) -> NodeId end, Trees),
    lists:sort(TreeIds)
  end,

  %% should include nodes without edges
  ?assertEqual([0, 1], FindTreeIds({2, []})),

  %% should include multi-level trees
  ?assertEqual([0], FindTreeIds({3, [
    {0, 1},
    {1, 2}
  ]})),

  %% should not include cycles branches
  ?assertEqual([], FindTreeIds({3, [
    {0, 1},
    {0, 2},
    {1, 2}
  ]})),

  %% should not include linked subsets
  ?assertEqual([], FindTreeIds({6, [
    {0, 1},
    {1, 2},

    {3, 4},
    {4, 5},

    {2, 5}
  ]})),

  %% should not include linked subsets
  ?assertEqual([0, 1, 6], FindTreeIds(wallapop_forest_spec())).

count_trees_test() ->
  CalculateForestTreesCount = fun(ForestSpec) ->
    Forest = magic_forest:new(ForestSpec),
    magic_forest:count_trees(Forest)
  end,

  %% should correctly handle empty forest
  ?assertEqual(0, CalculateForestTreesCount({0, []})),

  %% should correctly handle non empty forest
  ?assertEqual(2, CalculateForestTreesCount({2, []})),

  %% should pass Wallapop test case
  ?assertEqual(3, CalculateForestTreesCount(wallapop_forest_spec())).

is_tree_test_() ->
  Node = #node{},
  Forest = magic_forest:new({2, []}),

  {foreach,
    fun() -> meck:new(magic_forest, [passthrough]) end,
    fun(_) -> meck:unload(magic_forest) end,
    [
      {"when node is root and is branch", fun() ->
        meck:expect(magic_forest, is_root, fun(_) -> true end),
        meck:expect(magic_forest, is_branch, fun(_, _) -> true end),
        ?assertEqual(true, magic_forest:is_tree(Node, Forest)),
        ?assert(meck:validate(magic_forest))
      end},
      {"when node is root and is not branch", fun() ->
        meck:expect(magic_forest, is_root, fun(_) -> true end),
        meck:expect(magic_forest, is_branch, fun(_, _) -> false end),
        ?assertEqual(false, magic_forest:is_tree(Node, Forest)),
        ?assert(meck:validate(magic_forest))
      end},
      {"when node is not root and is branch", fun() ->
        meck:expect(magic_forest, is_root, fun(_) -> false end),
        meck:expect(magic_forest, is_branch, fun(_, _) -> true end),
        ?assertEqual(false, magic_forest:is_tree(Node, Forest)),
        ?assert(meck:validate(magic_forest))
      end},
      {"when node is not root and is not branch", fun() ->
        meck:expect(magic_forest, is_root, fun(_) -> false end),
        meck:expect(magic_forest, is_branch, fun(_, _) -> false end),
        ?assertEqual(false, magic_forest:is_tree(Node, Forest)),
        ?assert(meck:validate(magic_forest))
      end}
    ]
  }.

is_root_test() ->
  ?assertEqual(true, magic_forest:is_root(#node{parents = []})),
  ?assertEqual(false, magic_forest:is_root(#node{parents = [1]})).

is_branch_test() ->
  Forest = magic_forest:new({12, [
    {1, 2},

    {3, 4},
    {4, 5},
    {4, 6},

    {7, 8},
    {8, 10},
    {9, 10},
    {10, 11}
  ]}),

  %% should return true if node without edges
  ?assertEqual(true, magic_forest:is_branch(magic_forest:node(0, Forest), Forest)),

  %% should return true if node has no parents
  ?assertEqual(true, magic_forest:is_branch(magic_forest:node(1, Forest), Forest)),

  %% should return true if node has many children
  ?assertEqual(true, magic_forest:is_branch(magic_forest:node(4, Forest), Forest)),

  %% should return true if node has many nested children
  ?assertEqual(true, magic_forest:is_branch(magic_forest:node(3, Forest), Forest)),

  %% should return false if node has many parents
  ?assertEqual(false, magic_forest:is_branch(magic_forest:node(10, Forest), Forest)),

  %% should return false if node child is not branch
  ?assertEqual(false, magic_forest:is_branch(magic_forest:node(7, Forest), Forest)),

  %% should test for branch downward only
  ?assertEqual(false, magic_forest:is_branch(magic_forest:node(10, Forest), Forest)).

%% ---------------------------------------------------------------
%% Helper functions
%% ---------------------------------------------------------------

assert_node({ExpId, ExpParents, ExpChildren}, #node{id = ActId, parents = ActParents, children = ActChildren}) ->
  ?assertEqual(ExpId, ActId),
  ?assertEqual(lists:sort(ExpParents), lists:sort(ActParents)),
  ?assertEqual(lists:sort(ExpChildren), lists:sort(ActChildren)).

wallapop_forest_spec() ->
  {10, [
    {1, 2},
    {3, 4},
    {3, 5},
    {4, 5},
    {6, 7},
    {6, 8},
    {6, 9}
  ]}.
