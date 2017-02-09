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

The principle is to look for cycles by walking the graph depth-first. Everytime
a cycle is found, the edge with the greatest weight is removed. 

Note: this algorithm is more complex and less efficient than the Prim's algorithm, which will be implemented next (in 0107b.hs).

-------------------------------------------------------------------------------}

import Data.Array.IArray
import Data.Maybe
import Data.List

--------------------------------------------------------------------------------

type Weight = Int
type Node = Int
type Path = [Node]
type Cycle = [Node]

type Graph = Array (Node, Node) EdgeState

data EdgeState = None 
               | InGraph Weight 
               | Unused Weight 
               deriving Show


--------------------------------------------------------------------------------

-- Reduce a connected graph into a connected tree such that the sum of the
-- weight of the edges is minimum.
reduce :: Graph -> Graph
reduce graph = graph_iter [1] $ reset graph
  where
    graph_iter :: Path -> Graph -> Graph
    graph_iter path graph
        | null      path       = graph
        | isNothing mbNextNode = graph_iter (tail path) graph
        | isNothing mbCycle    = graph_iter updatedPath updatedGraph
        | otherwise            = graph_iter rewindedPath reducedGraph
      where
        lastNode = head path
        mbNextNode = pickUnusedEdgeFrom lastNode graph
        Just nextNode = mbNextNode
    
        updatedPath = nextNode : path
        mbCycle = getCycle updatedPath
        Just cycle = mbCycle
    
        updatedGraph = useEdge lastNode nextNode graph
    
        (fromNode, toNode) = locateEdgeToRemove cycle graph
        (rewindedPath, rewindedGraph) = rewindUpTo toNode path graph
        reducedGraph = removeEdge fromNode toNode rewindedGraph

-- Turn the edges from being "in the graph" to "free"
reset :: Graph -> Graph
reset graph = amap update graph
  where
    update None = None
    update (InGraph weight) = Unused weight 

-- Select an unused edge from a node and return the corresponding neightbour
-- node.
pickUnusedEdgeFrom :: Node -> Graph -> Maybe Node
pickUnusedEdgeFrom node graph 
    | null nodes = Nothing  
    | otherwise  = Just $ head nodes
  where
    nodes = [j | j <- [1..size graph], isUnused $ graph ! (node, j)]

--------------------------------------------------------------------------------

-- Return a graph from an array describing the weight of the edges between each
-- pair of nodes. A weight of zero indicates a lack of edge.
makeGraphFromArray :: Array (Int, Int) Int -> Graph
makeGraphFromArray = amap (\w -> if w == 0 then None else InGraph w)

-- Return the weight of the edge of a graph
weight :: Graph -> Weight
weight graph = (foldl' (\a e -> a + edge_weight e) 0 $ elems graph) `div` 2

size :: Graph -> Node
size = fst . snd . bounds

-- Check of edge is unused
isUnused (Unused _) = True
isUnused _ = False


useEdge :: Node -> Node -> Graph -> Graph 
useEdge n1 n2 graph = graph // [((n1, n2), edge'), ((n2, n1), edge')]
  where
    Unused weight = graph ! (n1, n2)
    edge' = InGraph weight

releaseEdge :: Node -> Node -> Graph -> Graph 
releaseEdge n1 n2 graph = graph // [((n1, n2), edge'), ((n2, n1), edge')]
  where
    InGraph weight = graph ! (n1, n2)
    edge' = Unused weight
   

removeEdge :: Node -> Node -> Graph -> Graph 
removeEdge n1 n2 graph = graph // [((n1, n2), None), ((n2, n1), None)]

edge_weight :: EdgeState -> Int
edge_weight None = 0
edge_weight (InGraph weight) = weight
edge_weight (Unused weight)  = weight

--------------------------------------------------------------------------------

-- Return the nodes making a cycle.  The first node and the last node of the
-- cycle are the same node.
getCycle :: Path -> Maybe Cycle
getCycle (n:ns)
    | isNothing mbCycleStart = Nothing
    | otherwise = Just (n : take (1 + fromJust mbCycleStart) ns)
  where
    mbCycleStart = elemIndex n ns
    
--------------------------------------------------------------------------------

locateEdgeToRemove:: Cycle -> Graph -> (Node, Node)
locateEdgeToRemove cycle graph 
    = snd $ maximumBy 
        (\(w1, _) (w2, _) -> compare w1 w2)
        [(edge_weight $ graph ! edge, edge) | edge <- zip cycle (tail cycle)]  

rewindUpTo :: Node -> Path -> Graph -> (Path, Graph)
rewindUpTo node path@(n:ns) graph
    | node == n = (path , graph)
    | otherwise = rewindUpTo node ns $ releaseEdge n (head ns) graph
  where
    m = head ns

--------------------------------------------------------------------------------

euler :: Graph -> IO()
euler graph = do
    let reducedGraph = reduce graph
    putStr $ "Initial weight: "; print $ weight graph
    putStr $ "Reduced weight: "; print $ weight reducedGraph
    putStr $ "Difference:     "; print $ weight graph - weight reducedGraph


graphSample :: Graph
graphSample = makeGraphFromArray 
            $ listArray ((1, 1), (7, 7)) [  0,  16,  12,  21,   0,   0,   0
                                         , 16,   0,   0,  17,  20,   0,   0
                                         , 12,   0,   0,  28,   0,  31,   0
                                         , 21,  17,  28,   0,  18,  19,  23
                                         ,  0,  20,   0,  18,   0,   0,  11
                                         ,  0,   0,  31,  19,   0,   0,  27
                                         ,  0,   0,   0,  23,  11,  27,   0]

readGraph :: IO(Graph)
readGraph = do
    input <- readFile ("data/p107_network.txt")
    
    let lines = Prelude.lines input

    let lines' = map (\l -> read ('[' : foldr update  "]" l)) lines :: [[Int]]
                    where update c = (:) (if c == '-' then '0' else c)

    let len = length lines
    return $ makeGraphFromArray $ listArray ((1, 1), (len, len)) $ concat lines'

main = do
  putStrLn "Sample\n------"
  euler graphSample

  putStrLn "Actual\n------"
  graph <- readGraph
  euler graph