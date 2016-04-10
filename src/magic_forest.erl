%%%-------------------------------------------------------------------
%%% @author vavaka
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(magic_forest).

-include("magic_forest.hrl").

%% API
-export([
  new/1,
  node/2,
  trees/1,
  count_trees/1
]).

%% ---------------------------------------------------------------
%% Public functions
%% ---------------------------------------------------------------

-spec new(Options :: forest_options()) -> forest().
new({Size, Edges}) ->
  NodeIds = lists:seq(0, Size - 1),
  Nodes = lists:foldl(fun(NodeId, NodesAcc) ->
    orddict:store(NodeId, #node{id = NodeId}, NodesAcc)
  end, orddict:new(), NodeIds),

  lists:foldl(fun({From, To}, NodesAcc) ->
    NodeFrom = #node{children = FromChildren} = orddict:fetch(From, NodesAcc),
    NodeTo = #node{parents = ToParents} = orddict:fetch(To, NodesAcc),

    NodesAccTmp = orddict:store(From, NodeFrom#node{children = lists:merge(FromChildren, [To])}, NodesAcc),
    orddict:store(To, NodeTo#node{parents = lists:merge(ToParents, [From])}, NodesAccTmp)
  end, Nodes, Edges).

-spec node(Id :: forest_node_id(), Forest :: forest()) -> forest_node().
node(Id, Forest) ->
  orddict:fetch(Id, Forest).

-spec trees(Forest :: forest()) -> list(forest_node()).
trees(Forest) ->
  orddict:fold(fun(_NodeId, Node, Acc) ->
    case is_tree(Node, Forest) of
      true -> [Node | Acc];
      false -> Acc
    end
  end, [], Forest).

-spec count_trees(Forest :: forest()) -> non_neg_integer().
count_trees(Forest) ->
  length(trees(Forest)).

%% ---------------------------------------------------------------
%% Private functions
%% ---------------------------------------------------------------

-spec is_tree(Node :: forest_node(), Forest :: forest()) -> boolean().
is_tree(Node = #node{}, Forest) ->
  is_root(Node) and is_branch(Node, Forest).

-spec is_root(Node :: forest_node()) -> boolean().
is_root(#node{parents = []}) ->
  true;
is_root(#node{}) ->
  false.

-spec is_branch(Node :: forest_node(), Forest :: forest()) -> boolean().
is_branch(#node{parents = Parents}, _Forest) when length(Parents) > 1 ->
  false;
is_branch(#node{children = Children}, Forest) ->
  lists:dropwhile(fun(ChildId) ->
    ChildNode = node(ChildId, Forest),
    is_branch(ChildNode, Forest)
  end, Children) =:= [].

