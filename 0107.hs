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

Implementation of Prim's algorithm.

-------------------------------------------------------------------------------}

import Data.Array.IArray
import Data.List

--------------------------------------------------------------------------------

type Weight = Int
type Node = Int
type Edges = Array (Node, Node) Weight

--------------------------------------------------------------------------------

-- Return the weight of the lightest tree spanning the given graph using Prim's
-- algo
prim :: Edges -> Weight 
prim edges = iter [1] (removeNode 1 (nodes edges)) 0 
  where
    iter :: [Node] -> [Node] -> Weight -> Weight
    iter inNodes outNodes acc  
        | null outNodes = acc
        | otherwise     = iter (j:inNodes) (removeNode j outNodes) (acc+w)
      where
        pairs = [(w, (i, j)) | i <- inNodes, j <- outNodes
                             , let w = edges ! (i, j)
                             , w /= 0] 
        (w, (i, j)) = head $ sort pairs

    removeNode :: Node -> [Node] -> [Node]
    removeNode node nodes = as ++ bs
      where 
        Just ix = elemIndex node nodes
        (as, (b:bs)) = splitAt ix nodes
        
-- Return the total weight of the given graph
weight :: Edges -> Weight
weight graph = div (foldl' (+) 0 $ elems graph) 2

-- Return the nodes of the given graph
nodes :: Edges -> [Node]
nodes graph = [1.. (fst $ snd $ bounds graph)]

--------------------------------------------------------------------------------

euler :: Edges -> IO()
euler graph = do
    let w = weight graph
    let w' = prim graph
    putStrLn $ "Initial weight: " ++ show w
    putStrLn $ "Reduced weight: " ++ show w'
    putStrLn $ "Difference:     " ++ show (w-w')

graphSample :: Edges
graphSample = listArray ((1, 1), (7, 7)) [  0,  16,  12,  21,   0,   0,   0
                                         , 16,   0,   0,  17,  20,   0,   0
                                         , 12,   0,   0,  28,   0,  31,   0
                                         , 21,  17,  28,   0,  18,  19,  23
                                         ,  0,  20,   0,  18,   0,   0,  11
                                         ,  0,   0,  31,  19,   0,   0,  27
                                         ,  0,   0,   0,  23,  11,  27,   0]

readGraph :: IO(Edges)
readGraph = do
    input <- readFile ("data/p107_network.txt")
    let lines = Prelude.lines input
    let lines' = map (\l -> read ('[' : foldr update  "]" l)) lines :: [[Int]]
                    where update c = (:) (if c == '-' then '0' else c)
    let len = length lines
    return $ listArray ((1, 1), (len, len)) $ concat lines'

main = do
  putStrLn "Sample\n------"
  euler graphSample

  putStrLn "Actual\n------"
  graph <- readGraph
  euler graph