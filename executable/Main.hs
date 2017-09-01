{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main (main) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Lsystem

exampleTurtle :: Turtle
exampleTurtle = Turtle {instr="FF-FF-FF-FF" , rules=[('F', "F-F-F")] , angle=90}

lines' :: Maybe (Trail' Line V2 Double)
lines' = fromOffsets <$> parse exampleTurtle

fig' :: Maybe (Diagram B)
fig' = strokeLine <$> lines'

main :: IO ()
main = maybe (print "Aww shucks") mainWith fig'
