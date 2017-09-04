{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Lsystem.Render
(
    parse
  , renderSystem
) where

import Lsystem.Generator
import Lsystem.Grammar

import Data.List (group)
import Diagrams.Prelude
import Diagrams.TwoD.Vector (e)
import Diagrams.Backend.SVG


parse :: [Node] -> Maybe [V2 Double]
parse = sequence . parse' 0 where
  parse' :: Double -> [Node] -> [Maybe (V2 Double)] 
  parse' a  []                       = []
  parse' a  [NodeRotate _ r _ _]     = []
  parse' a  [NodeDraw _ x]           = [Just $ e (a @@ deg)]
  parse' a ((NodeRotate _ r _ _):ns) =                          parse' (a + r) ns
  parse' a ((NodeDraw _ x):ns)       = [Just $ e (a @@ deg)] ++ parse'  a      ns
  parse' _ _ = [Nothing]

makeLines :: System -> Either String (Trail' Line V2 Double)
makeLines (System basis rules steps) =
  case (parse $ walk rules basis !! steps) of
    Just x  -> Right (fromOffsets x)
    Nothing -> Left "Mysterious parse failure"

renderSystem :: (Double,Double) -> String -> System -> IO ()
renderSystem (x,y) filename sys =
  either
    print
    (renderSVG filename (dims (V2 x y)))
    (strokeLine <$> makeLines sys)
