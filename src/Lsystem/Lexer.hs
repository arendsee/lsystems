module Lsystem.Lexer
(
  parseTurtle
) where

import Text.Parsec
import qualified Text.Parsec.Combinator as TPC
import qualified Text.Parsec.String as PS
import qualified Text.Parsec.Language as Lang
import qualified Text.Parsec.Token as Token
import qualified Text.Parsec.Char as C

import Lsystem.Syntax

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where
  style = Lang.emptyDef {
            Token.commentLine     = ""
          , Token.commentStart    = ""
          , Token.commentEnd      = ""
          , Token.nestedComments  = True
          , Token.identStart      = C.letter <|> char '_'
          , Token.identLetter     = alphaNum <|> oneOf "_'"
          , Token.opStart         = Token.opLetter Lang.emptyDef
          , Token.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
          , Token.reservedOpNames = [] -- TODO use it
          , Token.reservedNames   = []
          , Token.caseSensitive   = True
          }

parseNatural :: PS.Parser Integer
parseNatural = Token.natural lexer

parseFloat :: PS.Parser Double
parseFloat = Token.float lexer

parseIdentifier :: PS.Parser String
parseIdentifier = Token.identifier lexer

parseReservedOp :: String -> PS.Parser ()
parseReservedOp = Token.reservedOp lexer

parseTurtle :: String -> Either ParseError System
parseTurtle s = parse system' "" s

system' :: PS.Parser System
system' = do
  -- TODO make these optional
  n <- char 'n' >> equalSep >> parseNatural
  _ <- commaSep
  d <- char 'd' >> equalSep >> parseFloat

  -- -- TODO get a real preprocessor to handle this
  -- _ <- spaces
  -- m <- optionMaybe macro'
  -- _ <- spaces

  b <- char 'w' >> colonSep >> rhsPattern'

  _ <- spaces
  r <- many1 rule'
  
  return $ System {
      systemDepth = n
    , systemAngle = d
    , systemTable = [("hi", "bi")]
    , systemParam = TurtleParam { turtleParamIgnore = "-+" }
    , systemBasis = b
    , systemRules = r
  }

rule' :: PS.Parser Production
rule' = do
  lhs <- lhsPattern'
  _ <- C.spaces
  rhs <- rhsPattern'
  return $ Production {
      productionContext = ("","")
    , productionChance  = 1
    , productionFrom    = lhs
    , productionTo      = rhs
  }

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

-- TODO futher parse this
rhsPattern' :: PS.Parser [Char]
rhsPattern' = manyTill C.anyChar (C.char ';')



lhsPattern' :: PS.Parser [Char]
lhsPattern' = manyTill C.anyChar (try (string "->"))

commaSep :: PS.Parser ()
commaSep = C.spaces >> C.char ',' >> C.spaces

colonSep :: PS.Parser ()
colonSep = C.spaces >> C.char ':' >> C.spaces

equalSep :: PS.Parser ()
equalSep = C.spaces >> C.char '=' >> C.spaces

arrowSep :: PS.Parser ()
arrowSep = C.spaces >> C.char '-' >> C.char '>' >> C.spaces
