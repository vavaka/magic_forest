%%%-------------------------------------------------------------------
%%% @author vavaka
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-type forest()          :: orddict:dict().
-type forest_options()  :: {pos_integer(), [edge()]}.

-type forest_node_id()  :: non_neg_integer().
-type edge()            :: {forest_node_id(), forest_node_id()}.

-record(node, {
  id              :: forest_node_id(),
  parents   = []  :: list(),
  children  = []  :: list()
}).

-type forest_node() :: #node{}.