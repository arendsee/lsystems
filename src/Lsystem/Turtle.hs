module Lsystem.Turtle
(
    Turtle(..)
  , TurtleCode
  , parse
  , nextTurtle
  , turtlesForever
) where

import Lsystem.Generator

import Data.List (group)
import Diagrams
import Diagrams.TwoD.Vector (e)

type TurtleCode = String
type AngleMultiplier = Double

data Turtle = Turtle {
      instr :: TurtleCode
    , rules :: [(Char, TurtleCode)]
    , angle :: AngleMultiplier
  }
  deriving(Show, Eq, Ord)

nextTurtle :: Turtle -> Turtle
nextTurtle t = t { instr = next (rules t) (instr t) }

turtlesForever :: Turtle -> [Turtle]
turtlesForever t = [t] ++ turtlesForever (nextTurtle t)

data Visibility = Visible | Invisible deriving(Show, Eq, Ord)

parse :: Turtle -> Maybe [V2 Double]
parse t = sequence $ parse' (angle t) 0 (instr t) where

  -- TODO: manage visibility
  toVec :: Double -> Int -> Visibility -> V2 Double
  toVec k a Visible   = e (k * fromIntegral(a) @@ deg)
  toVec k a Invisible = e (k * fromIntegral(a) @@ deg)

  parse' :: Double -> Int -> TurtleCode -> [Maybe (V2 Double)]
  parse' k a ['F']    = [Just $ toVec k a Visible  ]
  parse' k a ['f']    = [Just $ toVec k a Invisible]
  parse' k a ['+']    = []
  parse' k a ['-']    = []
  parse' k a ('F':cs) = [Just $ toVec k a Visible  ] ++ parse' k a cs
  parse' k a ('f':cs) = [Just $ toVec k a Invisible] ++ parse' k a cs
  parse' k a ('+':cs) = parse' k (a + 1) cs
  parse' k a ('-':cs) = parse' k (a - 1) cs
  parse' _ _ _        = [Nothing]
