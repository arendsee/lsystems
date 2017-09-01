module Lsystem.Render
(
    asDiagram
  , renderTurtle
) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Lsystem.Turtle

asDiagram :: Turtle -> Diagram B
asDiagram = undefined

renderTurtle :: Diagram B -> IO ()
renderTurtle = undefined
