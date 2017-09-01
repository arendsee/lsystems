{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main (main) where

import Diagrams.Prelude
import Diagrams.Backend.SVG

import System.Environment as SE
import Data.List.Split as DLS
import Data.List as DL

import Lsystem

makeTurtle :: [String] -> Either String (Trail' Line V2 Double)
makeTurtle [n,angle',basis',rules']
  =   turtle' (parseDouble angle') basis' (parseRules rules')
  >>= lines' (parseInt n)
  where

    parseRules :: String -> Maybe [(Char, String)]
    parseRules = sequence . map DL.uncons . DLS.splitOn ";"

    parseDouble :: String -> Maybe Double
    parseDouble s = case reads s :: [(Double, String)] of
      [(x,"")] -> Just x
      _ -> Nothing

    parseInt :: String -> Maybe Int
    parseInt s = case reads s :: [(Int, String)] of
      [(x,"")] -> Just x
      _ -> Nothing

    turtle'
      :: Maybe Double
      -> String
      -> Maybe [(Char, String)]
      -> Either String Turtle
    turtle' Nothing _ _ = Left "Failed to parse angle"
    turtle' _ _ Nothing = Left "Failed to parse rules"
    turtle' (Just a) i (Just r) = Right $ Turtle { instr=i, rules=r, angle=a } 

    lines' :: Maybe Int -> Turtle -> Either String (Trail' Line V2 Double)
    lines' Nothing _ = Left "Failed to parse number of iterations"
    lines' (Just n') t =
      maybe 
        (Left "Failed to parse")
        Right 
        (fromOffsets <$> parse (turtlesForever t !! n'))
makeTurtle _ = Left "Expected cmdline: 'turtle <n> <angle> <basis> <rules>', found some dumb shit that wasn't even close."


makeFigure :: [String] -> Either String (Diagram B)
makeFigure ("turtle":args) = strokeLine <$> makeTurtle args
makeFigure (cmd:_) = Left ("Illegal command '" ++ cmd ++ "'")
makeFigure [] = Left "No arguments given, you're not even trying"

main :: IO ()
main = do
  args <- SE.getArgs 
  either print (renderSVG "foo.svg" (dims (V2 400 400))) (makeFigure args)
