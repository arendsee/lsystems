module Lsystem.Generator
(
    step
  , walk
  , transDol
  , maybeTransDol
  , transDolSys
  , ignoreContext
  , unconditional
) where

import Data.Maybe

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

maybeTransDol :: String -> Maybe [Node]
maybeTransDol = sequence . map translate'
  where
    translate' :: Char -> Maybe Node
    translate' 'F' = Just $ NodeDraw [] 1
    translate' '+' = Just $ NodeRotate [] 270 0 0
    translate' '-' = Just $ NodeRotate [] 90  0 0
    translate' _   = Nothing

transDol :: String -> [Node]
transDol = catMaybes . map translate'
  where
    translate' :: Char -> Maybe Node
    translate' 'F' = Just $ NodeDraw [] 1
    translate' '+' = Just $ NodeRotate [] 270 0 0
    translate' '-' = Just $ NodeRotate [] 90  0 0
    translate'  _  = Nothing

transDolSys :: String -> String -> Int -> System
transDolSys b r i = System {
      systemBasis = transDol b
    , systemRules = [fromF (transDol r)]
    , systemSteps = i
  } where

    isF :: Node -> Bool
    isF (NodeDraw _ _) = True
    isF _ = False

    fromF :: [Node] -> Rule
    fromF repl =
      DeterministicRule {
          ruleContext     = ignoreContext
        , ruleCondition   = unconditional
        , ruleMatch       = isF
        , ruleReplacement = repl
      }

ignoreContext :: LeftContext -> RightContext -> Bool
ignoreContext _ _ = True

unconditional :: LeftContext -> RightContext -> Node -> Bool
unconditional _ _ _ = True
