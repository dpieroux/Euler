{-------------------------------------------------------------------------------

The following undirected network consists of seven vertices and twelve edges
with a total weight of 243.

    B--20---E
   / \     / \
  16  17  18  11
 /     \ /     \
A--21---D--23---G
 \     / \     /
  12  28  19  27
   \ /     \ /
    C--31---F

The same network can be represented by the matrix below.
    A   B   C   D   E   F   G
A   -   16  12  21  -   -   -
B   16  -   -   17  20  -   -
C   12  -   -   28  -   31  -
D   21  17  28  -   18  19  23
E   -   20  -   18  -   -   11
F   -   -   31  19  -   -   27
G   -   -   -   23  11  27  -

However, it is possible to optimise the network by removing some edges and still
ensure that all points on the network remain connected. The network which
achieves the maximum saving is shown below. It has a weight of 93, representing
a saving of 243 − 93 = 150 from the original network.

    B       E
   / \     / \
  16  17  18  11
 /     \ /     \
A       D       G
 \       \      
  12      19    
   \       \  
    C       F

Using network.txt (right click and 'Save Link/Target As...'), a 6K text file
containing a network with forty vertices, and given in matrix form, find the
maximum saving which can be achieved by removing redundant edges whilst ensuring
that the network remains connected.

--------------------------------------------------------------------------------

The solution is to start with a null path and to add one of the nodes (the
initial node) to it.

Once a node is added to the path, let's call it the current node, its neighbours
are checked. If they are not yet connect to the path, they are also added to the
path through that node. If they are already part of a path, it is check if
connecting to the current node instead reduces the total path weight.

This is the case if
    - the neighbour is not already in the path of to the current node (to avoid
      making a cycle)
    - the weight of the edge from the current node to the neighbour is smaller
      that the weight of the immediate edge currently connecting the neighbour
      to initial node.

Whenever it is the case, the path to the neighbour is updated and the neighbours
of the updated neighbour are checked.

The process stops once there is no more checks to do.

Because requests to re-evaluate a node can accumulate before the node is re-
evaluated, a clock mechanism is set-up. A request to re-evaluate a node contains
the clock at which it was emitted; each node contains also the clock at which it
was updated last time (or 0 if the node was never updated yet). The node is
checked for an update only if the update request was emitted after the last
update of the node update.

-------------------------------------------------------------------------------}

import Data.Array.IArray
import Data.List(minimumBy)

maxInt = maxBound :: Int

type NodeIx = Int
type Weight = Int
type Clock = Int

data Node = Node { node_ancestors :: [NodeIx]
                 , node_edgeWeight :: Weight
                 , node_clk :: Clock 
                 } deriving Show

nullNode = Node [] maxInt (-1) 

data Edge = Edge { edge_nodeIx :: NodeIx
                 , edge_weight :: Weight
                 } deriving Show

type Graph = Array NodeIx [Edge]

type Nodes = Array NodeIx Node

data Update = Update { update_node :: NodeIx 
                     , update_clk  :: Clock
                     } deriving Show


minWeight :: Graph -> Weight
minWeight graph = sum $ map node_edgeWeight updatedNodes
--minWeight :: Graph -> Nodes
--minWeight graph = evolve nodes updates 1
  where
    (_, size) = bounds graph 
    nodes = array (1, size)
          $ (1, Node [] 0 0) : [(i, nullNode) | i <- [2..size]]
    updates = map (\edge -> Update (edge_nodeIx edge) 0) (graph ! 1)
    updatedNodes = elems $ evolve nodes updates 1

    evolve :: Nodes -> [Update] -> Clock -> Nodes
    evolve nodes [] _ = nodes
    evolve nodes ((Update uIx uClk):updates) clk 
        | needUpdate = evolve nodes' updates' (clk+1)
        | otherwise = evolve nodes updates clk
      where
        node = nodes ! uIx
        edges = graph ! uIx
        neighboursIx = map edge_nodeIx edges
        neighbours = map (nodes!) neighboursIx
        validAncestorsIx = filter (not . elem uIx . node_ancestors . (nodes!)) 
                                  neighboursIx
        validEdges = filter (flip elem validAncestorsIx . edge_nodeIx) edges
        minEdge = minimumBy (\a b -> compare (edge_weight a) (edge_weight b)) 
                            validEdges
        needUpdate = node_clk node < uClk 
                  && (edge_weight minEdge) < node_edgeWeight node

        fromIx = edge_nodeIx minEdge
        weight' = edge_weight minEdge
        ancestors' = fromIx : node_ancestors (nodes ! fromIx)
        node' = Node ancestors' weight' clk
        nodes' = nodes // [(uIx, node')]
        updates' = foldr (\ix acc -> (Update ix clk):acc) updates neighboursIx


edgesSample :: Graph
edgesSample = accumArray (\a b -> b:a) [] (1, 7)
            $ concat
            $ map (\(i, j, w) -> [(i, Edge j w), (j, Edge i w)])
                  [ (1, 2, 16), (1, 3, 12), (1, 4, 21)
                  , (2, 4, 17), (2, 5, 20)
                  , (3, 4, 28), (3, 6, 31)
                  , (4, 5, 18), (4, 6, 19), (4, 7, 23)
                  , (5, 7, 11)
                  , (6, 7, 27)]                      

