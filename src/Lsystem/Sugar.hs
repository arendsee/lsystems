{-|
Module      : Lsystem.Sugar
Description : Functions for simplifying L-system specification
Copyright   : (c) Zebulun Arendsee, 2018
License     : MIT
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Lsystem.Sugar
(
    transDol
  , ignoreContext
  , unconditional
  , maybeTransDol
  , transDolSys
  , transSolSys
  , matchF
  , matchDummy
  , matchWidth
  , contextMatch
  , similar
  , constantReplacement
) where

import Data.Maybe

import Lsystem.Grammar
import Lsystem.Generator

translate' :: Double -> Char -> Maybe Node
translate' _ 'F' = Just $ NodeDraw [] 1
translate' a '+' = Just $ NodeRotate [] (-1 * a) 0 0 -- '+' means clockwise
translate' a '-' = Just $ NodeRotate [] (     a) 0 0
translate' _  _  = Nothing

maybeTransDol :: Double -> String -> Maybe [Node]
maybeTransDol a s = sequence . map (translate' a) $ s

transDol :: Double -> String -> [Node]
transDol a s = catMaybes . map (translate' a) $ s

transDolSys :: Int -> Double -> String -> String -> System
transDolSys n angle basis replacement = System {
      systemBasis = transDol angle basis
    , systemRules = [fromF (transDol angle replacement)]
    , systemSteps = n
  } where

transSolSys :: Int -> Double -> String -> [(Chance, String)] -> System
transSolSys n angle basis replacements = System {
      systemBasis = transDol angle basis
    , systemRules = [StochasticRule (map (\(p, s) -> (p, mkRule s)) replacements)]
    , systemSteps = n
  } where
    mkRule :: String -> Rule 
    mkRule = fromF . transDol angle

matchF :: Node -> a -> Maybe a
matchF (NodeDraw _ _) x = Just x
matchF _ _ = Nothing

matchDummy :: String -> Node -> a -> Maybe a
matchDummy s1 (NodeDummy _ s2) x
  | s1 == s2  = Just x
  | otherwise = Nothing
matchDummy _ _ _ = Nothing

matchRotation :: Node -> a -> Maybe a
matchRotation (NodeRotate _ _ _ _) x = Just x
matchRotation _ _ = Nothing

matchWidth :: Node -> a -> Maybe a
matchWidth (NodeWidth _ _) x = Just x
matchWidth _ _ = Nothing 

unconditional :: LeftContext -> RightContext -> Node -> a -> Maybe a
unconditional _ _ _ x = Just x

-- | Check is A is a similar to B. For branches, A must be a subtree of B.
similar
  :: Node -- ^ A
  -> Node -- ^ B
  -> Bool -- ^ True if A is in B (where `in` is weirdly defined ...)
similar (NodeDummy _ s) (NodeDummy _ t) = s == t
similar (NodeDraw _ _ ) (NodeDraw _ _) = True
similar (NodeRotate _ _ _ _) (NodeRotate _ _ _ _) = True
similar (NodeBranch mss) (NodeBranch nss) = any id $ map (anyBranch nss) mss
  where

  anyBranch :: [[Node]] -> [Node] -> Bool
  anyBranch nss' ms' = any id $ map (similarBranch ms') nss'

  similarBranch
    :: [Node] -- A
    -> [Node] -- B
    -> Bool
  similarBranch [] _ = True  -- True if A is empty
  similarBranch _ [] = False -- False if B is empty
  similarBranch ns ms = all id $ zipWith similar ns ms
similar _ _ = False

contextMatch
  :: [Node] -- ^ elements to ignore
  -> [Node] -- ^ left contextual pattern
  -> [Node] -- ^ right contextual pattern
  -> [Node] -- ^ left context
  -> [Node] -- ^ right context
  -> a -> Maybe a -- combinator kludge (I really should go to Bool)
contextMatch ss lpat rpat lc rc x =
  return x
    >>= contextMatch' ss lc lpat
    >>= contextMatch' ss rc rpat
  where

  contextMatch'
    :: [Node] -- elements to ignore
    -> [Node] -- context (whether it is left or right doesn't matter)
    -> [Node] -- context to match
    -> a -> Maybe a -- combinator boilerplate (why not just use a Bool?)
  contextMatch' _  _      []     x = Just x
  contextMatch' _  []     _      _ = Nothing
  contextMatch' ss (c:cs) (m:ms) x
    | any id $ map (similar c) ss = contextMatch' ss cs (m:ms) x 
    | similar m c = contextMatch' ss cs ms x 
    | otherwise = Nothing

ignoreContext :: LeftContext -> RightContext -> a -> Maybe a
ignoreContext _ _ x = Just x

constantReplacement :: [Node] -> [Node] -> [Node] -> Node -> [Node]
constantReplacement ns _ _ _ = ns

--------------------------
-- local utility functions
--------------------------

fromF :: [Node] -> Rule
fromF repl =
  DeterministicRule {
      ruleContext     = ignoreContext
    , ruleCondition   = unconditional
    , ruleMatch       = matchF
    , ruleReplacement = constantReplacement repl
  }
