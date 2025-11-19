{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

module Graph where

import qualified Data.IntMap.Strict as IM
import qualified Data.Map as M
import qualified Data.IntSet as IS
import qualified Data.Sequence as Seq
import Data.List
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Function (on)
import Data.Maybe
import Debug.Trace
import qualified Data.Vector.Unboxed as VU
import Control.Applicative

type Node = Int
type Graph = IM.IntMap NodeSet

type NodeSet = IS.IntSet

parseFile :: String -> IO Graph
parseFile = liftM ((\(_,_,g) -> g) . foldl' (\(!acc) inp -> maybe acc (buildGraph acc) (parseEdge inp)
                                            ) (M.empty,IM.empty,IM.empty) . T.lines) . TIO.readFile

addEdge :: Node -> Node -> Graph -> Graph
addEdge !n1 !n2 = IM.insertWith (flip IS.union) n2 (IS.singleton n1) . IM.insertWith (flip IS.union) n1 (IS.singleton n2)

-- | assumes edges are of the format: node1 node2 or e node1 node2
parseEdge :: T.Text -> Maybe (T.Text, T.Text)
parseEdge inp = case T.words inp of
                    [e,v1,v2] -> guard (T.head e == 'e') >> return (v1,v2)
                    [v1,v2] -> guard (T.head v1 `notElem` ['#']) >> return (v1,v2) -- # starts a comment, could clash with single nodes in DIMACS format e.g.: e v1
                    _ -> Nothing

buildGraph :: (M.Map T.Text Int, IM.IntMap T.Text, Graph) -> (T.Text, T.Text) -> (M.Map T.Text Int, IM.IntMap T.Text, Graph)
buildGraph a@(!m,!rm,!g) (v1,v2) = if v1 == v2 then a else (m3,rm3,addEdge k k2 g) -- we don't allow self loops
  where (!m2,!rm2,!k) = getMapping v1 m rm
        (!m3,!rm3,!k2) = getMapping v2 m2 rm2
        getMapping node m rm = case M.lookup node m of
                                Nothing -> let nkey = M.size m in (M.insert node nkey m, IM.insert nkey node rm,nkey)
                                (Just n) -> (m,rm,n)

addEdge' :: Node -> Node -> Graph -> Graph
addEdge' n1 n2 = IM.insertWith (flip IS.union) n2 (IS.singleton n1) . IM.insertWith (flip IS.union) n1 (IS.singleton n2)

delNode :: Node -> Graph -> Graph
delNode n g = IM.delete n . IS.foldl'(delEdge' n) g $ getNbrs n g
  where delEdge' :: Node -> Graph -> Node -> Graph -- saves one adjust computation, edge is going to be deleted anyway via IM.delete n
        delEdge' toDel graph node = IM.adjust (IS.delete toDel) node graph

delEdge :: Node -> Node -> Graph -> Graph
delEdge n1 n2 = IM.adjust (IS.delete n2) n1 . IM.adjust (IS.delete n1) n2

nodecount :: Graph -> Int
nodecount = IM.size

getNbrs :: Node -> Graph -> NodeSet
getNbrs = IM.findWithDefault IS.empty

neighborhood :: NodeSet -> Graph -> NodeSet
neighborhood s g = flip (IS.\\) s $ IS.foldl' (\(!acc) v -> IS.union acc (getNbrs v g)) IS.empty s

getInducedGraph :: NodeSet -> Graph -> Graph
getInducedGraph s g = IS.foldl' (\(!acc) n -> IM.insert n (s `IS.intersection` getNbrs n g) acc) IM.empty s

buildInducedGraph :: Node -> Graph -> Graph -> Graph
buildInducedGraph n ig g = if IS.null diff
                           then IM.insert n IS.empty ig
                           else IS.foldl' (flip (addEdge' n)) ig diff 
                          where !inn = getNodes ig
                                !nbrs = getNbrs n g
                                !diff = inn `IS.intersection` nbrs

getNodes :: Graph -> NodeSet
getNodes = IM.keysSet

getNodesL :: Graph -> [Node]
getNodesL = IM.keys

regular :: Graph -> Bool
regular g = maxDeg g == minDeg g

degree :: Node -> Graph -> Int
degree n g = IS.size $ getNbrs n g

maxDeg :: Graph -> Int
maxDeg = IM.foldl' (\(!acc) s -> max acc (IS.size s)) 0

minDeg :: Graph -> Int
minDeg g = IM.foldl' (\(!acc) s -> min acc (IS.size s)) (IM.size g) g

minDeg' :: Graph -> (Int, Node)
minDeg' = minimumBy (compare `on` fst) . getDegrees -- foldlWithKey

maxDeg' :: Graph -> (Int, Node)
maxDeg' = maximumBy (compare `on` fst) . getDegrees -- foldlWithKey

getDegrees :: Graph -> [(Int,Node)]
getDegrees = IM.foldlWithKey' (\acc k s -> (IS.size s, k):acc) []

getDegrees' :: Graph -> [Int]
getDegrees' g = map (`degree` g) $ getNodesL g

getNodesByDegree :: NodeSet -> Graph -> [Node]
getNodesByDegree ns graph = IM.foldlWithKey (\xs k x -> x ++ xs) [] dmap
          where dmap :: IM.IntMap [Node]
                dmap = IS.foldl'(\acc n -> IM.insertWithKey f (IS.size $ getNbrs n graph) [n] acc) IM.empty ns
                f _ [new_val] old_val = new_val : old_val


-- in descending order
degreeList :: Graph -> [(Int,Node)]
degreeList = sortBy (flip compare `on` fst) . getDegrees

-- in ascending order
degreeListA :: Graph -> [(Int,Node)]
degreeListA = sortBy (compare `on` fst) . getDegrees

noOfEdges :: Graph -> Int
noOfEdges g = IM.foldl' f 0 g `div` 2
            where f !len !s = len + IS.size s

density :: Graph -> Double
density g = if IM.size g > 1
            then 2 * fromIntegral (noOfEdges g) / fromIntegral (noOfNodes * (noOfNodes - 1))
            else 0
            where !noOfNodes = IM.size g

density' :: Int -> Int -> Double
density' !edges !nodes = if nodes > 1
                         then 2 * fromIntegral edges / fromIntegral(nodes * (nodes - 1))
                         else 1


incCount' :: Int -> IM.IntMap Int -> IM.IntMap Int
incCount' i = IM.insertWith (+) i 1


maxNbrs :: Graph -> Int
maxNbrs = IM.foldl' (\(!acc) s -> max acc (IS.size s)) 0

minNbrs :: Graph -> Int
minNbrs = floor . IM.foldl' (\(!acc) s -> min acc $! fromIntegral (IS.size s)) (1/0)


-- | computes the degeneracy ordering of the graph, see: Matula & Beck (1983), Smallest-last ordering and clustering and graph coloring algorithms
degeneracy' :: Graph -> (Int, IM.IntMap Double)
degeneracy' !graph = go degMap IM.empty graph 0
  where !degMap = IM.foldlWithKey'(\(!nm) (!node) set -> IM.insertWith IS.union (IS.size set) (IS.singleton node) nm
                                  ) IM.empty graph
        go :: IM.IntMap IS.IntSet -> IM.IntMap Double -> Graph -> Int -> (Int, IM.IntMap Double)
        go dm res !g !k
          | IM.null g = (k,res)
          | otherwise = go dm' (IM.insert minNode (fromIntegral k') res) (delNode minNode g) k'
            where ((!minDeg, nodes), tmpdm) = fromMaybe ((0,IS.empty),IM.empty) (IM.minViewWithKey dm)
                  !k' = max k minDeg
                  (minNode, tmpdm') = let (val', rval) = fromMaybe (0, IS.empty) (IS.minView nodes) -- default case never happens
                                        in if IS.null rval
                                           then (val', tmpdm)
                                           else (val', IM.insert minDeg rval tmpdm)
                  dm' = IS.foldl'(\tdm node -> let !dn = degree node g
                                                   tdm' = IM.update (update' node) dn tdm
                                                   tdm'' = IM.insertWith IS.union (dn-1) (IS.singleton node) tdm'
                                               in tdm''
                                 ) tmpdm' (getNbrs minNode g)
                  update' :: Node -> IS.IntSet -> Maybe IS.IntSet
                  update' node val = let newval = IS.delete node val
                                     in if IS.null newval
                                        then Nothing
                                        else Just newval 
-- | same as above, but with added tie breaker (look at the extended neighborhood of a node)
degeneracy2 :: Graph -> (Int, IM.IntMap Double)
degeneracy2 !graph = go degMap IM.empty graph 0
  where !degMap = IM.foldlWithKey'(\(!nm) (!node) set -> IM.insertWith IS.union (IS.size set) (IS.singleton node) nm
                                  ) IM.empty graph
        !edges = noOfEdges graph
        !avgdeg = fromIntegral (2 * edges) / fromIntegral (IM.size graph)
        go :: IM.IntMap IS.IntSet -> IM.IntMap Double -> Graph -> Int -> (Int, IM.IntMap Double)
        go dm res !g !k
          | IM.null g = (k,res)
          | otherwise = go dm' (IM.insert minNode (fromIntegral k' + fromIntegral nbrs / fromIntegral edges) res) (delNode minNode g) k'
            where ((!minDeg, nodes), tmpdm) = fromMaybe ((0,IS.empty),IM.empty) (IM.minViewWithKey dm)
                  !k' = max k minDeg
                  (minNode, tmpdm',nbrs) = let (c, nbrs) = fromMaybe (0,0) (candidate nodes)
                                               rval = IS.delete c nodes
                                           in if IS.null rval
                                              then (c, tmpdm, nbrs)
                                              else (c, IM.insert minDeg rval tmpdm, nbrs)
                  dm' = IS.foldl'(\tdm node -> let !dn = degree node g
                                                   !tdm' = IM.update (update' node) dn tdm
                                                   !tdm'' = IM.insertWith IS.union (dn-1) (IS.singleton node) tdm'
                                               in tdm''
                                 ) tmpdm' (getNbrs minNode g)
                  update' :: Node -> IS.IntSet -> Maybe IS.IntSet
                  update' node val = let !newval = IS.delete node val
                                     in if IS.null newval
                                        then Nothing
                                        else Just newval
                  candidate :: IS.IntSet -> Maybe (Node, Int)
                  candidate = IS.foldl'(\(!acc) n -> let !nbrs = nbrsOfInterest n
                                                     in maybe (Just (n,nbrs)) (\(n',nbrs') -> if nbrs > nbrs'
                                                                                              then Just (n, nbrs)
                                                                                              else acc
                                                                              ) acc
                                       ) Nothing
                  nbrsOfInterest :: Node -> Int
                  nbrsOfInterest n = let nbrs = getNbrs n g
                                     in IS.foldl'(\(!acc') n' -> acc' + IS.size(getNbrs n' g `IS.intersection` nbrs)
                                                  ) (IS.size nbrs) nbrs


degeneracy :: Graph -> Int
degeneracy = fst . degeneracy'

degeneracyOrdering :: Graph -> IM.IntMap Double
degeneracyOrdering = snd . degeneracy'


bfs :: Node -> Graph -> NodeSet
bfs node graph = go (Seq.singleton node) IS.empty (IS.singleton node)
  where go :: Seq.Seq Node -> NodeSet -> NodeSet -> NodeSet
        go q v _
          | q == Seq.empty = v
        go (Seq.viewl -> x Seq.:< xs) v av = go (IS.foldl'(Seq.|>) xs (diff v x)) (IS.insert x v) (av `IS.union` diff v x)
                                                where diff v n = (getNbrs n graph IS.\\ v) IS.\\ av

tree :: Node -> Node -> [Node] -> NodeSet -> Graph -> Maybe NodeSet
tree node active vps p graph = go (IS.foldl' (Seq.|>) Seq.empty (IS.delete active $ getNbrs node graph)) (IS.singleton node) node
  where candidates = IS.fromList vps
        go :: Seq.Seq Node -> NodeSet -> Node -> Maybe NodeSet
        go q v _
          | q == Seq.empty = Just v
        go (Seq.viewl -> x Seq.:< xs) v last = if    (x `IS.member` v)
                                                  || (x `IS.member` p)
                                                  || (x `IS.member` candidates)
                                                then Nothing
                                                else go (IS.foldl' (Seq.|>) xs (diff x)) (IS.insert x v) x
                                                       where diff n = IS.delete active . IS.delete last $ getNbrs n graph

getComponents :: Graph -> [NodeSet]
getComponents graph
  | graph == IM.empty = []
  | otherwise = go (Just x) [] xs
    where (x:xs) = getNodesL graph
          go :: Maybe Node -> [NodeSet] -> [Node] -> [NodeSet]
          go Nothing cl _ = cl
          go (Just n) cl rest = go (safeHead next) (lst n:cl) next
                                where next = rest \\ IS.toList (lst n)
                                      lst !n = bfs n graph

isClique :: NodeSet -> Graph -> Bool
isClique ns graph = k*(k-1) == IS.foldl'(\(!acc) n -> acc + IS.size (getNbrs n graph `IS.intersection` ns)) 0 ns
      where !k = IS.size ns

greedyColoring :: NodeSet -> NodeSet -> Graph -> Int
greedyColoring ns p graph = coloring (getNodesByDegree ns graph) p graph

coloring :: [Node] -> NodeSet -> Graph -> Int
coloring ns p graph = go ns IM.empty 0 IS.empty
        where
          go :: [Node] -> IM.IntMap Int -> Int -> IS.IntSet -> Int
          go [] cmap !count _ = count
          go (n:ns) cmap !count cset = let findColors = IS.foldl' (\acc (!nbr) -> case IM.lookup nbr cmap of
                                                                                 Just c -> IS.delete c acc
                                                                                 Nothing -> acc
                                                                  ) cset (getNbrs n graph IS.\\ p) -- frueher abbrechen, wenn cset leer
                                           colorToAssign = head $ IS.toList findColors
                                           !ncount = count + 1
                                       in if IS.size cset == 0 || IS.size findColors == 0
                                          then go ns (IM.insert n ncount cmap) ncount (IS.insert ncount cset)
                                          else go ns (IM.insert n colorToAssign cmap) count cset


coloringSL :: [Node] -> Int -> IM.IntMap Int -> NodeSet -> Graph -> Int
coloringSL ns k nc p g = let (cmap,cset) = color ns
                         in IS.size cset
    where 
          color :: [Node] -> (IM.IntMap Int, IS.IntSet)
          color = foldl'(\acc@(cmap,cset) node -> if not (IM.member node nc) then acc else -- nc consists of all nodes V \ P
                                                          case if IS.size cset == 0 then Nothing else findColor node cset cmap of
                                                            Nothing -> let !ncount = IS.size cset + 1
                                                                       in (IM.insert node ncount cmap, IS.insert ncount cset)
                                                            Just c -> (IM.insert node c cmap, cset)
                        ) (IM.empty, IS.empty)
          findColor :: Node -> IS.IntSet -> IM.IntMap Int -> Maybe Int
          findColor node cset cmap = IS.findMin <$> foldM (\acc nbr -> let acc' = maybe acc (`IS.delete` acc) (IM.lookup nbr cmap)
                                                                       in if IS.size acc' > 0
                                                                          then Just acc' -- $ maybe acc (`IS.delete` acc) (IM.lookup nbr cmap)
                                                                          else Nothing
                                                          ) cset (IS.toList $ getNbrs node g IS.\\ p)

coloringSL2 :: [Node] -> NodeSet -> Int -> Graph -> Int
coloringSL2 ns p k g = let (cmap,cset) = color ns
                     in IS.size cset
    where 
          color :: [Node] -> (IM.IntMap Int, IS.IntSet)
          color = foldl'(\acc@(cmap,cset) node -> if not (IM.member node g) then acc else 
                                                  case if IS.size cset == 0 then Nothing else findColor node cset cmap of
                                                    Nothing -> let !ncount = IS.size cset + 1
                                                               in (IM.insert node ncount cmap, IS.insert ncount cset)
                                                    Just c -> (IM.insert node c cmap, cset)
                        ) (IM.empty, IS.empty)
          findColor :: Node -> IS.IntSet -> IM.IntMap Int -> Maybe Int
          findColor node cset cmap = IS.findMin <$> foldM (\acc nbr -> let acc' = maybe acc (`IS.delete` acc) (IM.lookup nbr cmap)
                                                                       in if IS.size acc' > 0
                                                                          then Just acc' -- $ maybe acc (`IS.delete` acc) (IM.lookup nbr cmap)
                                                                          else Nothing
                                                          ) cset (IS.toList $ getNbrs node g)

decomposeGraph :: Graph -> [Graph]
decomposeGraph graph = if length components > 1
                       then foldl' (\(!acc) n -> getInducedGraph n graph : acc) [] components
                       else [graph]
              where components = getComponents graph

-- helper

safeMaximumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
safeMaximumBy _ [] = Nothing
safeMaximumBy ord xs = Just (maximumBy ord xs)

safeHead :: [a] -> Maybe a
safeHead = listToMaybe