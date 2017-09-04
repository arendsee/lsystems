module Lsystem.Generator
(
    step
  , walk
  , fromF
  , ignoreContext
  , unconditional
  , is90
  , is270
  , o90
  , o270
  , isF
  , oF
  , oSquare
) where

import Lsystem.Grammar

-- TODO replace with a real random choice function
choose :: [(Chance, a)] ->  Maybe a
choose ((_,x):_) = Just x
choose _ = Nothing

applyRules :: LeftContext -> RightContext -> [Rule] -> Node -> [Node]
applyRules lc rc []     n = [n]
applyRules lc rc (r:rs) n = case applyRule n lc rc r of
  Nothing -> applyRules lc rc rs n
  Just x  -> x
  where
    applyRule :: Node -> LeftContext -> RightContext -> Rule -> Maybe [Node]
    applyRule n lc rc (DeterministicRule cont cond match repl) =
      case (match n && cont lc rc && cond lc rc n) of
        True  -> Just repl
        False -> Nothing
    applyRule n lc rc (StochasticRule rs) = case (choose rs) of
      Just r -> applyRule n lc rc r
      Nothing -> Nothing

-- this will need to be refitted to allow branching later
step :: [Rule] -> [Node] -> [Node]
step _ [] = []
step rs xs = concat $ step' rs [] xs where
  step' :: [Rule] -> [Node] -> [Node] -> [[Node]]
  step' _  _  []     = []
  step' rs lc [r]    = [applyRules lc [] rs r]
  step' rs lc (r:rc) = [applyRules lc rc rs r] ++ step' rs (r:lc) rc

walk :: [Rule] -> [Node] -> [[Node]]
walk rs n = [n] ++ walk' rs n where
  walk' rs n = [next'] ++ walk' rs next' where
    next' = step rs n

ignoreContext :: LeftContext -> RightContext -> Bool
ignoreContext _ _ = True

unconditional :: LeftContext -> RightContext -> Node -> Bool
unconditional _ _ _ = True

isF :: Node -> Bool
isF (NodeDraw _ _) = True
isF _ = False

is90 :: Node -> Bool
is90 (NodeRotate _ 90 0 0) = True
is90 _ = True

is270 :: Node -> Bool
is270 (NodeRotate _ 270 0 0) = True
is270 _ = True

fromF :: [Node] -> Rule
fromF repl =
  DeterministicRule {
      ruleContext     = ignoreContext
    , ruleCondition   = unconditional
    , ruleMatch       = isF
    , ruleReplacement = repl
  }

oF :: Node -- F
oF = NodeDraw [] 1

o90 :: Node -- -
o90 = NodeRotate [] 90  0 0

o270 :: Node  -- +
o270 = NodeRotate [] 270 0 0

oSquare :: [Node] -- F-F-F-F
oSquare = [
      oF     -- F
    , o270   -- -
    , oF     -- F
    , o270   -- -
    , oF     -- F
    , o270   -- -
    , oF     -- F
  ]
