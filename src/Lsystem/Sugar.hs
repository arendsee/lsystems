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

ignoreContext :: LeftContext -> RightContext -> a -> Maybe a
ignoreContext _ _ x = Just x

unconditional :: LeftContext -> RightContext -> Node -> a -> Maybe a
unconditional _ _ _ x = Just x

matchF :: Node -> a -> Maybe a
matchF (NodeDraw _ _) x = Just x
matchF _ _ = Nothing

matchDummy :: String -> Node -> a -> Maybe a
matchDummy s1 (NodeDummy s2) x | s1 == s2  = Just x
                               | otherwise = Nothing
matchDummy _ _ _ = Nothing

--------------------------
-- local utility functions
--------------------------

fromF :: [Node] -> Rule
fromF repl =
  DeterministicRule {
      ruleContext     = ignoreContext
    , ruleCondition   = unconditional
    , ruleMatch       = matchF
    , ruleReplacement = repl
  }
