module Lsystem.Generator
(
    step
  , walk
) where

import qualified System.Random as SR
import qualified Control.Monad.Trans.State as TS

import Lsystem.Grammar

choose :: SR.StdGen -> [(Chance, a)] -> (SR.StdGen, Maybe a)
choose g rs =  case (SR.randomR (0.0, 1.0) g) of
  (p, g') -> (g', choose' p (cumulative 0 rs)) where

    cumulative :: Chance -> [(Chance, a)] -> [(Chance, a)]
    cumulative _ []  = [] 
    cumulative p [(q, x)]    = [(p + q, x)] 
    cumulative p ((q, x):rs) = [(p + q, x)] ++ cumulative (p + q) rs

    choose' :: Chance -> [(Chance, a)] -> Maybe a
    choose' _ [] = Nothing
    choose' p ((q, x):rs')
      | p < 0 || p > 1 = error "Invalid random generator: probability values must be between 0 and 1"
      | q < 0 || q > 1 = error "Invalid stochastic rule: probability must be between 0 and 1"
      | p <= q = Just x
      | otherwise = choose' p rs'

applyRules :: SR.StdGen -> LeftContext -> RightContext -> [Rule] -> Node -> [Node]
applyRules _ _  _  []     n = [n]
applyRules g lc rc (r:rs) n = case applyRule g n lc rc r of
  (g', Nothing) -> applyRules g lc rc rs n
  (_, Just x) -> x
  where
    applyRule :: SR.StdGen -> Node -> LeftContext -> RightContext -> Rule -> (SR.StdGen, Maybe [Node])
    applyRule g' n lc rc (DeterministicRule cont cond match repl) =
      case (match n && cont lc rc && cond lc rc n) of
        True  -> (g', Just repl)
        False -> (g', Nothing)
    applyRule g' n lc rc (StochasticRule rs) = case (choose g' rs) of
      (g'', Just r) -> applyRule g'' n lc rc r
      (g'', _) -> (g'', Nothing)

rContextMap :: SR.StdGen -> (SR.StdGen -> [a] -> [a] -> a -> b) -> [a] -> [a] -> [b]
rContextMap _ _ _ [] = []
rContextMap g f lc (r:rc) = [f g lc rc r] ++ rContextMap (snd $ SR.next g) f (r:lc) rc

step :: [Rule] -> SR.StdGen -> [Node] -> [Node] -> Node -> [Node]
step rs g lc rc (NodeBranch nss) = [NodeBranch (map desc' nss)] where
  desc' :: [Node] -> [Node]
  desc' = concat . rContextMap g (step rs) lc
step rs g lc rc r = applyRules g lc rc rs r

walk :: [Rule] -> SR.StdGen -> [Node] -> [[Node]]
walk rs g ns = [ns] ++ walk' rs g ns where
  walk' rs g' ns = [next'] ++ future' where
    gs      = SR.split g'
    next'   = concat $ rContextMap (fst gs) (step rs) [] ns
    future' = walk' rs (snd gs) next'


contexts :: [a] -> [[a]]
contexts [] = [[]]
contexts (x:xs) = [x:xs] ++ contexts xs 


data W = W {
      wGen :: SR.StdGen
    , wLeft :: [Node]
    , wRight :: [Node]
    , wCurrent :: Node
    , wRule :: [Rule]
    , wNextGen :: [Node]
  }

emptyW :: W = W {
      wGen     = SR.mkStdGen 0
    , wLeft    = []
    , wRight   = []
    , wRule    = []
    , wNextGen = []
  }

initW :: Int -> System -> W
initW seed (System _ []  _) = emptyW { wGen = SR.mkStdGen seed }
initW seed (System ns rs _) = emptyW {
      wGen     = SR.mkStdGen seed
    , wRight   = ns
    , wRule    = rs
  }

branch :: W -> State W [[Node]]

step :: W -> State W [Node]

getRule :: W -> State W Rule

applyRule :: Rule -> W -> State W Rule


-- step :: W -> Maybe W
-- step (W _   _  [] _ _ ) -> Nothing
-- step (W gen lc rc (NodeBranch nss) rs) -> undefined
-- step (W gen lc rc c rs) -> undefined
--
--
-- mkBranch :: W -> [[Node]] -> W
-- mkBranch w nss = w {wGen = g', wLeft =
--
-- mkBranch w [] = w
-- mkBranch (W gen lc rc c rs) nss = W {
--       wGen :: SR.StdGen
--     , wLeft :: [Node]
--     , wRight :: [Node]
--     , wCurrent :: Node
--     , wRule :: [Rule]
--   }
