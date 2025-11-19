{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

{-
Implementation for:
Finding Connected Subgraphs of Fixed Minimum Density: Implementation and Experiments
-}

module Main where

import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Sequence as Seq
import Data.List
import Data.Maybe
import Graph
import System.Environment
import Control.Monad
import Control.Applicative
import System.FilePath.Posix
import Data.Time
import Data.Function
import System.Timeout
import qualified Data.Text.Read as TR
import qualified Data.Text as T
import System.Console.GetOpt
import System.IO
import System.Exit
import System.Directory (doesFileExist)
import Control.Monad.Trans.Maybe
import Control.Monad.State.Strict hiding (modify')
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.PQueue.Min as MQ
import qualified Data.PQueue.Max as MAQ
import qualified Data.Vector.Unboxed as VU
import Debug.Trace (trace)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import System.IO (withFile, IOMode(WriteMode), hPutStrLn)
import Control.Monad (forM_)
import Data.List (intercalate)
import qualified Data.IntSet as IS

{-# NOINLINE foundCliquesRef #-}
foundCliquesRef :: IORef [([Int], Double)]
foundCliquesRef = unsafePerformIO (newIORef [])

{-# NOINLINE accumulate #-}
accumulate :: [Int] -> Double -> Bool -> Bool
accumulate clique density shouldAppend = unsafePerformIO $ do
    when shouldAppend $ atomicModifyIORef' foundCliquesRef (\xs -> ((clique, density) : xs, ()))
    return True

writeCliquesToFile :: FilePath -> IO ()
writeCliquesToFile path = do
    cliques <- readIORef foundCliquesRef
    withFile path WriteMode $ \h ->
        mapM_ (\(clique, density) -> hPutStrLn h $ unwords (map show clique) ++ " " ++ show density) (reverse cliques)


data Result = Found {-# UNPACK #-} !Double {-# UNPACK #-} !Int | Solved {-# UNPACK #-} !Int | Aborted {-# UNPACK #-} !Int

printResult :: Result -> Int -> String
printResult (Solved !steps) !k = "solved " ++ show k ++ " " ++ show steps
printResult (Aborted !steps) !k = "aborted " ++ show k ++ " " ++ show steps
printResult (Found !density !steps) !k = "found " ++ show k ++ " " ++ show steps ++ " " ++ show density

run :: Graph -> Options -> IO Result
run graph opts = maybe (go 2) runK (optK opts)
  where go :: Int -> IO Result
        go !k = runK k >>= \res -> case res of
                                        (Found _ _) -> go (k+1)
                                        res' -> return res'
        graphs = decomposeGraph graph
        gscore = map(\g -> let ordering = scores g opts
                               orderedNodes = let ord = fromMaybe IM.empty ordering
                                              in if isNothing ordering
                                                 then getNodesByDegree (getNodes g) g
                                                 else sortBy(flip compare `on` (\n -> ord IM.! n)) (getNodesL g)
                           in (g,ordering,if optTwins opts then mkTwins g else mkEmptyTwins, orderedNodes)
                    ) graphs
        runK :: Int -> IO Result
        runK k = do
                    start <- getCurrentTime
                    (!res,!steps) <- runStateT (run' k (fromMaybe 0 (optMu opts)) gscore opts) 0
                    stop <- getCurrentTime
                    let !t = diffUTCTime stop start
                    putStrLn (printResult (res steps) k ++ " " ++ show t) >> return (res steps)


-- | The Graph is disassembled into its connected components (run). Nodes of a connected component are sorted and feed into the algorithm
-- | one at a time (run', the pivot nodes). This processing can be done in parallel. A job consists of the pivot node and its context.

hoistState :: Monad m => State s a -> StateT s m a
hoistState = StateT . (return .) . runState

run' :: Int -> Double -> [(Graph,Scoring,Twins,[Node])] -> Options -> StateT Int IO (Int -> Result)
run' !k !μ graphs opts = hoistState (go graphs)
    where
      go :: [(Graph,Scoring,Twins,[Node])] -> State Int (Int -> Result)
      go [] = return Solved
      go ((g,score,twins,orderedNodes):gs) = do
                    step <- runMaybeT $ go' (sortByScore score Nothing (getNodesL g) g opts 0) g score twins orderedNodes
                    case step of
                      Nothing -> go gs
                      Just d -> return $ Found d
      go' :: [Node] -> Graph -> Scoring -> Twins -> [Node] -> MaybeT (State Int) Double
      go' [] _ _ _ _ = mzero
      go' (n:ns) g o tw on
--        | k == 2 && nodecount g >= k = return 1.0
        | nodecount g < k || fromIntegral (noOfEdges g) < noOfEdgesNeeded = mzero
        | otherwise = delta k μ g o n opts tw on <|> go' ns (IS.foldl'(flip delNode) (delNode n g) $ getTwins n tw) o tw on -- (scores g opts) delete all twins
        where !noOfEdgesNeeded = μ * fromIntegral k * fromIntegral(k-1) / 2
              !nodeCount = IS.size (getNodes g)

-- | for legacy reasons, not needed with newer Control.Monad packages (ghc >= 7.6)
modify' :: MonadState s m => (s -> s) -> m ()
modify' f = state (\(!s) -> ((), f s))

initNbrCount :: Node -> Graph -> NbrCount
initNbrCount node = IM.delete node . IM.foldlWithKey'(\(!acc) n nbrs -> IM.insert n (if node `IS.member` nbrs then 1 else 0) acc) IM.empty

-- | Data structures for the different states.

data Solution = Solution { p :: NodeSet, n :: NodeSet, edgesP :: {-# UNPACK #-} !Int }

data GraphState = GraphState { g :: Graph, edgesG :: {-# UNPACK #-} !Int, edgesVP :: {-# UNPACK #-} !Int, k1 :: NodeSet }

data TreeNode = TreeNode { s :: Solution, gs :: GraphState, nc :: NbrCount }

mkTreeNode :: Node -> Graph -> TreeNode
mkTreeNode v g' = TreeNode { s = Solution { p = IS.singleton v, n = IS.empty, edgesP = 0 }
                           , gs = GraphState { g = g', edgesG = noOfEdges g', edgesVP = degree v g', k1 = IS.empty } 
                           , nc = initNbrCount v g' }

extendP :: Node -> TreeNode -> TreeNode
extendP v tn@TreeNode { s = s', gs = gs', nc = nc' } = let !nip = getNbrsInP v nc'
                                                           s'' = s' { p = IS.insert v (p s'), edgesP = edgesP s' + nip }
                                                           !evp = degree v (g gs') - nip
                                                       in tn { s = s'', nc = updateNbrCount v nc' (g gs'), gs = gs' { edgesVP = edgesVP gs' - nip + evp } }

deleteNodes :: TreeNode -> NodeSet -> TreeNode
deleteNodes tn@TreeNode { s = s', gs = gs', nc = nc' } ns = let (nc'',g'',!edges'',!edgesVP'') = IS.foldl'(\(!nc,!g,!edges'',!edgesVP'') node -> (IM.delete node nc, delNode node g, edges'' - degree node g, edgesVP'' - getNbrsInP node nc)
                                                                                                          ) (nc',g gs',edgesG gs', edgesVP gs') ns
                                                            in tn { gs = gs' {g = g'', edgesG = edges'', edgesVP = edgesVP'' }, nc = nc'' }

extendN :: Node -> TreeNode -> TreeNode
extendN v tn@TreeNode { s = s', gs = gs', nc = nc' } = let toDelete = getNbrs v (g gs') IS.\\ p s'
                                                           tn' = deleteNodes tn toDelete
                                                       in tn' { s = s' { n = IS.insert v (n s') } }

extend1Core :: NodeSet -> TreeNode -> TreeNode
extend1Core ns tn@TreeNode { s = s', gs = gs', nc = nc' } = tn { gs = gs' { k1 = k1 gs' `IS.union` ns } }

-- | helper functions to access elements of a search tree node
k1Size :: TreeNode -> Int
k1Size = IS.size . k1 . gs

pivotSize :: TreeNode -> Int
pivotSize = IS.size . p . s

nodecount' :: TreeNode -> Int
nodecount' = IM.size . g . gs

-- | The algorihtm as described in Section 1 of the paper (see README). See also extendP and extendN.
delta :: Int -> Double -> Graph -> Maybe (IM.IntMap Double) -> Node -> Options -> Twins ->  [Node] -> MaybeT (State Int) Double
delta !k !μ graph degordering !pivot opts tw orderedNodes = go (Just pivot) (sortByScore degordering Nothing (IS.toList $ getNbrs pivot graph) graph opts 1) (mkTreeNode pivot graph)
    where go :: Maybe Node -> [Node] -> TreeNode -> MaybeT (State Int) Double
          go (Just _) _ tn
            | let cliqueNodes = map (+1) $ IS.toList (p $ s tn)
                , trace (if pdensity >= μ
                        then "cal found: " ++ show cliqueNodes ++ " with density = " ++ show pdensity
                        else "") True
                , accumulate cliqueNodes pdensity (pdensity >= μ)
                , k == lp && pdensity >= μ
                    = mzero
            | k == lp && pdensity < μ = mzero
            | nodecount' tn < klp = mzero
            | k1Size tn + lp >= k && density' (edges + klp) k >= μ = return (density' (edges + klp) k)
    --        | not (edgeBound μ k (nodecount' tn) (edgesG $ gs tn)) = mzero
            | l > klp && notFeasible opts k lp edges μ (p $ s tn) (g $ gs tn) (nc tn) l orderedNodes = mzero
                where !lp = pivotSize tn
                      !edges = edgesP $ s tn
                      !pdensity = density' edges lp
                      !klp = k - lp
                      !l = ceiling $ (μ * fromIntegral (k * (k-1)) / 2.0) - fromIntegral edges
          go Nothing [] tn = case active' (p $ s tn) (n $ s tn) of
                               Nothing -> mzero
                               Just a -> go (Just a) (sortedNbrs a) tn
                                        where sortedNbrs a = sortByScore degordering (Just (`getNbrsInP` (nc tn))) (IS.toList $ (getNbrs a (g $ gs tn) IS.\\ (p $ s tn)) IS.\\ (k1 $ gs tn)) (g $ gs tn) opts (pivotSize tn)
          go (Just pivot) [] tn = lift (modify' (+1)) >> go Nothing [] (extendN pivot tn)
          go pivot@(Just piv) (v:vps) tn = lift (modify' (+1)) >>
                                           case if opt1Core opts && getNbrsInP v (nc tn) == 1 then tree v piv vps (p $ s tn) (g $ gs tn) else Nothing of
                                              (Just ns) ->  go pivot vps (extend1Core ns tn)
                                              Nothing   ->  let tn' = extendP v tn
                                                            in go pivot (sortByScore degordering (Just (`getNbrsInP` (nc tn'))) vps (g $ gs tn') opts (pivotSize tn')) tn' -- (extendP v tn)
                                                        <|> go pivot (if IS.null toDelete then vps else filter(\n -> not (IS.member n toDelete)) vps) (deleteNodes tn (IS.insert v toDelete))
                                                              where toDelete = getTwins v tw IS.\\ (p $ s tn)

removeDegree1 :: Node -> NbrCount -> Graph -> (Int, (NbrCount,Graph))
removeDegree1 v nc g = let remove = IS.filter (\node -> degree node g == 1 && getNbrsInP node nc == 1) $ getNbrs v g
                       in (IS.size remove, IS.foldl'(\(!nc,!g) node -> (IM.delete node nc, delNode node g)) (nc,g) remove)


getNbrsInP :: Node -> NbrCount -> Int
getNbrsInP n nc = nc IM.! n

getNbrsInP' :: Node -> NbrCount -> Int
getNbrsInP' n nc = fromMaybe 0 (IM.lookup n nc)

-- | Bounds: Check if it's feasible to obtain a μ-Clique with the current solution. See Section 3 of the paper.
notFeasible :: Options -> Int -> Int -> Int -> Double -> NodeSet -> Graph -> NbrCount -> Int -> [Node] -> Bool
notFeasible opts !k !lp !edges !μ p graph nc l ns = (optSB opts && not (densityFeasible edges lp k μ))  ||
                                                    (optPB opts && not (densityFeasibleWithP edges lp k μ nc)) ||
                                                    (optOPB opts && not (outerPB edges lp k μ nc graph l)) ||
                                                    (optIPB opts && not (innerPB edges lp k μ nc graph l)) ||
                                                    (optTB opts && not (turanBound' edges lp k μ p graph nc ns)) ||
                                                    (optPPB opts && not (pairwisePB edges lp k μ p nc graph l))
                                                  --  (optTB opts && not (turanBound edges lp k μ p graph nc))

edgeBound :: Double -> Int -> Int -> Int -> Bool
edgeBound !μ !k !n !m = fromIntegral k <= (μ + 2.0 * sqrt((μ + 2.0)^2 + 8.0 * fromIntegral(m - n) * μ)) / (2.0 * μ)

type Scoring = Maybe (IM.IntMap Double)

-- | The scores are calculated once beforehand. If both degeneracy and degree sort is enabled, then we choose the degeneracy sort
-- | iff the degeneracy is smaller than 1/10th of the maximum degree.
scores :: Graph -> Options -> Scoring
scores graph opts 
    | regular graph = Nothing
    | (optDegSort opts && optDegenSort opts) || optScoreSort opts = if fromIntegral degen < 1 / 10 * fromIntegral (maxDeg graph) then Just degordering else Nothing
    | optDegenSort opts = Just degordering
    | otherwise = Nothing
    where (!degen, !degordering) = if optNbrhood opts then degeneracy2 graph else degeneracy' graph
          nbrhood = sortNbrhood graph

sortByScore :: Scoring -> Maybe (Node -> Int) -> [Node] -> Graph -> Options -> Int -> [Node]
sortByScore ordering nbrsinp ns g opts p = sortBy ((if optPesSort opts then id else flip) compare `on` score) ns
  where score :: Node -> Double
        score !n = let !degen = maybe 0.0 (IM.! n) ordering
                       !deg = if isJust ordering || not (optDegSort opts) then 0 else degree n g
                       !nip = fromMaybe (const 0) nbrsinp n
                   in degen + fromIntegral (deg + nip)

sortNbrhood :: Graph -> IM.IntMap Double
sortNbrhood g = IM.foldlWithKey'(\(!acc) n nbrs -> let !count = IS.foldl'(\(!acc') n' -> acc' + IS.size(getNbrs n' g `IS.intersection` nbrs)
                                                                         ) (IS.size nbrs) nbrs
                                                   in IM.insert n (fromIntegral count) acc
                                ) IM.empty g


twins :: Graph -> Int
twins = length . twinsClasses

twins' :: Graph -> [(Node,Node)]
twins' g = [(x,y) | let list = IS.toList (getNodes g), x <- list, y <- list, x < y, getNbrs x g == getNbrs y g] 

-- List of twin classes, includes the open and closed neighborhood. Possible to prune twins of a fruitless (already processed) nodes.
twinsClasses :: Graph -> [NodeSet]
twinsClasses g = snd $ IM.foldlWithKey'(\acc@(tw,cl) n nbrs ->
                                             if n `IS.member` tw
                                             then acc
                                             else let nnbrs = IS.insert n nbrs
                                                      (tw',cl') = IS.foldl'(\acc@(tw,cl) n' ->
                                                                            let nbrs' = getNbrs n' g
                                                                                nnbrs' = IS.insert n' nbrs'
                                                                            in if n /= n' && (nbrs == nbrs' || (IS.size nbrs > 1  && nnbrs == nnbrs'))
                                                                               then (IS.insert n' tw, IS.insert n' cl)
                                                                               else acc
                                                                           ) (tw,IS.empty) $ getNodes g IS.\\ tw
                                                  in if IS.null cl'
                                                     then acc
                                                     else (IS.insert n tw', IS.insert n cl':cl)
                                 ) (IS.empty, []) g

data Twins = Twins { toClass :: IM.IntMap Int, classes :: IM.IntMap NodeSet } deriving Show

mkTwins :: Graph -> Twins
mkTwins g = let classes = twinsClasses g
                cmap = IM.fromAscList $ zip [1..] classes
                toclass = IM.foldlWithKey'(\(!m) cl ns -> IS.foldl'(\(!acc) n -> IM.insert n cl acc) m ns) IM.empty cmap
            in Twins { toClass = toclass, classes = cmap }

mkEmptyTwins :: Twins
mkEmptyTwins = Twins { toClass = IM.empty, classes = IM.empty }

getTwins :: Node -> Twins -> NodeSet
getTwins n tw = case IM.lookup n (toClass tw) of
                    Nothing -> IS.empty
                    (Just cl) -> classes tw IM.! cl

turanBound :: Int -> Int -> Int -> Double -> NodeSet -> Graph -> NbrCount -> Bool
turanBound !edges !nodes !k !μ p graph dc = μ <= newedges / fromIntegral (k * (k-1))
          where
            !newedges = 2 * (fromIntegral (edges + top) + tedges)
            !top = if IM.null dc
                   then k' * IS.size p
                   else toDescList k' dc
            !r = greedyColoring (getNodes graph IS.\\ p) p graph
            !k' = k - nodes
            !tedges = (1.0 - 1.0 / fromIntegral r) * (fromIntegral k'^2 / 2.0)

-- | same as turanBound but with a different coloring: exploits the degeneracy ordering
turanBound' :: Int -> Int -> Int -> Double -> NodeSet -> Graph -> NbrCount -> [Node] -> Bool
turanBound' !edges !nodes !k !μ p graph dc ns = μ <= newedges / fromIntegral (k * (k-1))
          where
            !newedges = 2 * (fromIntegral (edges + top) + tedges)
            !top = if IM.null dc
                   then k' * IS.size p
                   else toDescList k' dc
            !r = coloringSL ns k dc p graph
            !k' = k - nodes
            !tedges = (1.0 - 1.0 / fromIntegral r) * (fromIntegral k'^2 / 2.0)

densityFeasible :: Int -> Int -> Int -> Double -> Bool
densityFeasible !edges !nodes !k !μ = μ <= (fromIntegral (2 * (edges + sum [nodes..k-1])) / fromIntegral (k * (k-1)))
    where k' = k - nodes 

-- | See Section 3, Proposition 4 in the paper.
densityFeasibleWithP :: Int -> Int -> Int -> Double -> NbrCount -> Bool
densityFeasibleWithP !edges !nodes !k !μ dc = μ <= (fromIntegral (2 * (edges + toDescList k' dc + k' * (k'-1) `div` 2)) / fromIntegral (k * (k-1)))
   where k' = k - nodes

type NbrCount = IM.IntMap Int

updateNbrCount :: Node -> NbrCount -> Graph -> NbrCount
updateNbrCount v nc g = IM.delete v $ IS.foldl' (flip incCount) nc (getNbrs v g)

incCount :: Node -> NbrCount -> NbrCount
incCount = IM.adjust (+ 1)

toDescList :: Int -> IM.IntMap Int -> Int
toDescList k = sum . MQ.toList . IM.foldlWithKey'(\q (!node) (!nbrs) -> if MQ.size q < k
                                                                         then MQ.insert nbrs q
                                                                         else if MQ.findMin q < nbrs
                                                                              then MQ.insert nbrs $ MQ.deleteMin q
                                                                              else q
                                                    ) MQ.empty


toAscList :: Int -> NodeSet -> IM.IntMap Int -> [(Node,Int)]
toAscList minimum p = sortBy (compare `on` snd) . filter(\(n,nbrs) -> nbrs > 0 && nbrs <= minimum + 1) . IM.toList

-- | See Section 3, Proposition 2 in the paper (see README).
innerPB :: Int -> Int -> Int -> Double -> NbrCount -> Graph -> Int -> Bool
innerPB !edges !nodes !k !μ nc g l = let !k' = k - nodes
                                         check ep = id
                                     in edgesToP k' l id (innerPB' g k') (IM.toList nc)

innerPB' :: Graph -> Int -> Node -> Int -> Int
innerPB' g k node nbrs = let !outsideP = min (degree node g - nbrs) (k-1)
                         in nbrs + outsideP

pairwisePB :: Int -> Int -> Int -> Double -> NodeSet -> NbrCount -> Graph -> Int -> Bool
pairwisePB !edges !nodes !k !μ p nc g l = let check ep = ep `div` (k' - 1)
                                              !k' = k - IS.size p
                                              !kpOver2 = k' * (k' - 1) `div` 2
                                              thereom1 :: Node -> Node -> Int
                                              thereom1 u v = let nu = getNbrs u g IS.\\ p
                                                                 nv = getNbrs v g IS.\\ p
                                                                 !isect = IS.size (nu `IS.intersection` nv)
                                                                 !i = min isect (k' - 2)
                                                                 !uv = if u `IS.member` nv then 2 else 0 
                                                                 !diag = IS.size nu + IS.size nv - 2 * isect - uv
                                                                 !d = min diag (max 0 (k' - 2 - isect))
                                                             in 2 * getNbrsInP' u nc + 2 * getNbrsInP' v nc + 2 * i + d + uv
                                              in (k' < 2) || edgesToP kpOver2 l check thereom1 [(x,y) | let list = IS.toList (getNodes g IS.\\ p), x <- list, y <- list, x < y]


toDegList :: Int -> Graph -> NbrCount -> Int
toDegList k g = MQ.foldrU(+) 0 . IM.foldlWithKey'(\q (!node) (!nbrs) -> let !maximize = 2*nbrs + outsideP
                                                                            !outsideP = min (degree node g - nbrs) (k-1)
                                                                        in if MQ.size q < k
                                                                           then MQ.insert maximize q
                                                                           else if MQ.findMin q < maximize
                                                                                then MQ.insert maximize $ MQ.deleteMin q
                                                                                else q
                                                  ) MQ.empty

-- | Helper for the different bounds, allows for early termination.
edgesToP :: Int -> Int -> (Int -> Int) -> (Node -> Int -> Int) -> [(Node,Int)] -> Bool
edgesToP size l bound maxi = isNothing . foldM(\a'@(!acc, q) (node,nbrs) -> let !maximize = maxi node nbrs
                                                                                (!acc',q') = if MQ.size q < size
                                                                                             then (acc + maximize, MQ.insert maximize q)
                                                                                             else let (m, q') = MQ.deleteFindMin q
                                                                                                  in if m < maximize
                                                                                                     then (acc - m + maximize, MQ.insert maximize $ MQ.deleteMin q)
                                                                                                     else a'
                                                                            in if bound acc' >= l then Nothing else Just (acc', q')
                                                 ) (0,MQ.empty)

-- | See Section 3, Proposition 3 in the paper.
outerPB :: Int -> Int -> Int -> Double -> NbrCount -> Graph -> Int -> Bool
outerPB !edges !nodes !k !μ nc g l = let !k' = k - nodes
                                         check ep = id
                                     in edgesToP k' l id (outerPB' g k') (IM.toList nc)

outerPB' :: Graph -> Int -> Node -> Int -> Int
outerPB' g k node nbrs = let !outsideP = min (degree node g - nbrs) (k-1)
                         in 2 * nbrs + outsideP


data Options = Options
     { optHelp :: Bool
     , optK :: Maybe Int
     , optMu :: Maybe Double
     , optTimeout :: Maybe Int
     , optSB :: Bool
     , optPB :: Bool
     , optTB :: Bool
     , optOPB :: Bool
     , optPPB :: Bool
     , optIPB :: Bool
     , optDegSort :: Bool
     , optDegenSort :: Bool
     , optNbrSort :: Bool
     , optScoreSort :: Bool
     , optPesSort :: Bool
     , optTwins :: Bool
     , opt1Core :: Bool
     , optNbrhood :: Bool
     } deriving Show

defaultOptions :: Options
defaultOptions = Options
     { optHelp = False
     , optK = Nothing
     , optMu = Nothing
     , optTimeout = Nothing
     , optSB = True -- False
     , optPB = False
     , optTB = False
     , optOPB = False
     , optPPB = False
     , optIPB = False
     , optDegSort = False
     , optDegenSort = False
     , optNbrSort = False
     , optScoreSort = False
     , optPesSort = False
     , optTwins = False
     , opt1Core = False
     , optNbrhood = True -- False
     }

options :: [OptDescr (Options -> Options)]
options =
  [
    Option [] ["help"]
      (NoArg (\ opts -> opts { optHelp = True })) "print this help"
  , Option [] ["pb","simple-p-bound"]
      (NoArg (\ opts -> opts { optPB = True })) "enable simple P-Bound"
  , Option [] ["ipb","inner-p-bound"]
      (NoArg (\ opts -> opts { optIPB = True })) "enable Inner P-Bound"
  , Option [] ["tb","turan-bound"]
      (NoArg (\ opts -> opts { optTB = True })) "enable Turan Bound"
  , Option [] ["opb","outer-p-bound"]
      (NoArg (\ opts -> opts { optOPB = True })) "enable Outer P-Bound"
  , Option [] ["ppb","pairwise-p-bound"]
      (NoArg (\ opts -> opts { optPPB = True })) "enable Pairwise P-Bound"      
  , Option [] ["sort-by-degree"]
      (NoArg (\ opts -> opts { optDegSort = True })) "sort by degree"
  , Option [] ["sort-by-degeneracy"]
      (NoArg (\ opts -> opts { optDegenSort = True })) "sort by degeneracy"
  , Option [] ["sort-by-nbrs-in-p"]
      (NoArg (\ opts -> opts { optNbrSort = True })) "sort by neighbors in P"
  , Option [] ["sort-optimistic"]
      (NoArg (\ opts -> opts { optScoreSort = True })) "sorts differently depending on graph properties (same as: sort-by-degree and sort-by-degeneracy combined)"
  , Option [] ["sort-pessimistic"]
      (NoArg (\ opts -> opts { optPesSort = True })) "inverts the ordering"    
  , Option [] ["score"]
      (NoArg (\ opts -> opts { optPB = True, optScoreSort = True, optNbrSort = True })) "sorts by score and nbrs in P, simple P-Bound enabled"
  , Option [] ["twins"]
      (NoArg (\ opts -> opts { optTwins = True })) "enable Twins-Rule"
  , Option [] ["disable-nbrhood"]
      (NoArg (\ opts -> opts { optNbrhood = False })) "disable Neighborhood Tie-Breaker"
  , Option [] ["k"]
      (OptArg ((\ f opts -> opts { optK = f }) . parseInt . fromMaybe "")
                "k")
         "k (solution size), if ommited: k will be continuously increased, starting with k = 2"
  , Option [] ["t","timeout"]
      (OptArg ((\ f opts -> opts { optTimeout = f }) . parseInt . fromMaybe "")
                "t")
         "timeout (s), if ommited: no timeout is set"
  , Option [] ["mu"]
      (ReqArg ((\ f opts -> opts { optMu = f }) . parseDouble)
                "m")
         "minimum density of the solution"
  ]

parseInt :: String -> Maybe Int
parseInt s = case TR.decimal (T.pack s) of
                Right (n,"") -> Just n
                _ -> Nothing

parseDouble :: String -> Maybe Double
parseDouble s = case TR.double (T.pack s) of
                Right (n,"") -> Just n
                _ -> Nothing

parseOpts :: [String] -> IO (Options, String)
parseOpts argv =
  case getOpt Permute options argv of
    (args,[fs],[]) -> do
        exists <- doesFileExist fs
        unless exists $ hPutStrLn stderr (failure fs) >> exitFailure
        let nargs = foldl (flip id) defaultOptions args
        when (optHelp nargs) $ ioError (userError (usageInfo header options)) >> exitSuccess
        case optMu nargs of
              Nothing -> hPutStrLn stderr "invalid μ supplied " >> exitFailure
              (Just μ) -> if 0 <= μ && μ <= 1 then return (nargs, fs) else hPutStrLn stderr "invalid μ supplied: has to be between 0 and 1 " >> exitFailure
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: connected_mu_clique [OPTION...] file"
        failure file = "File \"" ++ file ++ "\" doesn't exist"

--  ghc -O2 -fexcess-precision -funfolding-use-threshold=16 -o connected_mu_clique connected_mu_clique.hs -threaded -rtsopts

main :: IO ()
main = do
  (flags, file) <- getArgs >>= parseOpts
  graph <- parseFile file
  void (timeout (fromMaybe (-1) (optTimeout flags) * 10^6) (run graph flags))
  writeCliquesToFile "output.txt"
  return ()

-- helper
active' :: NodeSet -> NodeSet -> Maybe Node
active' p n = safeHead $ IS.elems (p IS.\\ n)
