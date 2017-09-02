module Lsystem.Lexer
(
  parseTurtle
) where

import Control.Monad
import qualified Text.Parsec as TP
import qualified Text.Parsec.String as TPS
import qualified Text.Parsec.Char as TPC

import Lsystem.Syntax

parseTurtle :: String -> Either TP.ParseError System
parseTurtle s = TP.parse system' "" s

system' :: TPS.Parser System
system' = do
  _ <- TP.many TPC.anyChar
  return $ System {
      systemDepth = 0
    , systemAngle = 1.1
    , systemTable = [("hi", "bi")]
    , systemParam = TurtleParam { turtleParamIgnore = "-+" }
    , systemBasis = "FF"
    , systemRules = []
  }

-- identifier' :: Parser String
-- identifier' = undefined
--
-- rhsPattern' :: Parser [LhsElement]
-- rhsPattern' = string
--
-- lhsPattern' :: Parser String
-- lhsPattern' = identifier'
--
-- semicolonSep :: Parser String
-- semicolonSep = spaces >> char ':' >> spaces
--
-- equalSep :: Parser String
-- equalSep = spaces >> char '=' >> spaces
--
-- arrowSep :: Parser String
-- arrowSep = spaces >> char '-' >> char '>' >> spaces
--
-- macro' :: Parser [String]
-- macro' = many (char '#' >> spaces >> string)
--
-- system' :: Parser System
-- system' = do
--   n <- char 'n' >> semicolonSep >> integer
--   _ <- spaces
--   d <- char 'd' >> equalSep >> number
--   _ <- spaces
--   m <- option macro'
--   _ <- spaces
--   b <- char 'w' >> semicolonSep >> pattern'
--   _ <- spaces
--   r <- many1 rule'
--   return $ System {
--     systemDepth = n,
--     systemAngle = d,
--     systemTable = tableFromMacro m,
--     systemParam = paramFromMacro m,
--     systemBasis = b
--     systemRules = r
--   }
--
-- rule' :: Parser Rule
-- rule' = do
--   id <- identifier'
--   context <- option (semicolonSep >> context')
--   chances <- option (semicolonSep >> chances')
--   _ <- semicolonSep
--   lhs <- lhsPattern'
--   _ <- arrowSep
--   rhs <- rhsPattern'
--   return $ Rule {
--         id      = id
--       , context = context
--       , chances = chances
--       , lhs     = lhs
--       , rhs     = rhs
--     }
--
-- context' :: Parser [Context]
-- context' = do
--   a  <- lhsPattern'
--   o1 <- spaces >> oneOf "<>"
--   b  <- spaces >> lhsPattern'
--   o2 <- option (spaces >> ondeOf "<>")
--   c  <- option (spaces >> lhsPattern')
--   let con1 = case o1 of
--     '<' -> [ContextLhs a]
--     '>' -> [ContextRhs b]
--     _   -> []
--   let con2 = case (o2, c) of
--     (Just '<', Just _) -> [ContextLhs b]
--     (Just '>', Just _) -> [ContextLhs c]
--     _ -> []
--   return $ con1 ++ con2
--
-- chances' :: Parser Chances
-- chances' = undefined
