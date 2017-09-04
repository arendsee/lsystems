module Lsystem.Generator
(
    step
  , walk
  , ignoreContext
  , unconditional
) where

import qualified System.Random as SR

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

-- this will need to be refitted to allow branching later
step :: SR.StdGen -> [Rule] -> [Node] -> [Node]
step _ _ [] = []
step g rs xs = concat $ step' g rs [] xs where
  step' :: SR.StdGen -> [Rule] -> [Node] -> [Node] -> [[Node]]
  step' _ _  _  []      = [[]]
  step' g' rs lc [r]    = [applyRules g' lc [] rs r]
  step' g' rs lc (r:rc) = [applyRules g1 lc rc rs r] ++ step' g2 rs (r:lc) rc where
    gs = SR.split g'
    g1 = fst gs
    g2 = snd gs

walk :: SR.StdGen -> [Rule] -> [Node] -> [[Node]]
walk g rs n = [n] ++ walk' g rs n where
  walk' g' rs n = [next'] ++ future' where
    gs      = SR.split g'
    next'   = step  (fst gs) rs n
    future' = walk' (snd gs) rs next'

ignoreContext :: LeftContext -> RightContext -> Bool
ignoreContext _ _ = True

unconditional :: LeftContext -> RightContext -> Node -> Bool
unconditional _ _ _ = True
