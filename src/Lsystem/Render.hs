{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Lsystem.Render
(
  renderSystem
) where

import Lsystem.Generator
import Lsystem.Grammar

import Diagrams.Prelude
import Diagrams.TwoD.Vector
import Diagrams.Backend.SVG
import qualified System.Random as SR

diagramSystem :: SR.StdGen -> System -> Diagram B
diagramSystem g (System basis rules steps) = diagramPacman pacman' where
  pacman' = foldl eat pacman0 nodes'
  nodes'  = walk rules g basis !! steps

renderSystem :: SR.StdGen -> (Double,Double) -> String -> System -> IO ()
renderSystem g (x,y) filename sys =
  renderSVG filename (dims (V2 x y)) $ diagramSystem g sys


-- This is an internal datastructure for passing state across the fold
data Pacman = Pacman {
    pacmanStart :: P2 Double
  , pacmanEnd :: P2 Double
  , pacmanAngle :: Angle Double
  , pacmanSpawn :: [Pacman]
  , pacmanVectors :: [V2 Double]
}

pacman0 :: Pacman
pacman0 = Pacman {
      pacmanStart   = p2 (0,0) :: P2 Double
    , pacmanEnd     = p2 (0,0) :: P2 Double
    , pacmanAngle   = 90 @@ deg :: Angle Double
    , pacmanSpawn   = []
    , pacmanVectors = []
  }

spawnPacman :: Pacman -> Pacman
spawnPacman p = Pacman {
      pacmanStart   = pacmanEnd p
    , pacmanEnd     = pacmanEnd p
    , pacmanAngle   = pacmanAngle p
    , pacmanSpawn   = []
    , pacmanVectors = []
  }

eat :: Pacman -> Node -> Pacman 
eat t (NodeRotate _ r _ _) = t { pacmanAngle = pacmanAngle t <> (r @@ deg) }
eat t (NodeDraw _ x) = t {
      pacmanEnd = pacmanEnd t # translate v'
      -- build backwards for performance reasons, this will need to be reversed later
    , pacmanVectors = v' : (pacmanVectors t)
  } where
    v' = e (pacmanAngle t) # scale x
eat t (NodeDummy _) = t
eat t (NodeBranch nss) =
  t { pacmanSpawn = pacmanSpawn t ++ map spawn nss } where 
    spawn :: [Node] -> Pacman
    spawn [] = spawnPacman t
    spawn ns = foldl eat (spawnPacman t) ns

diagramPacman :: Pacman -> Diagram B
diagramPacman p = mkDia p <> mergeSpawn p where
  -- reverse the vectors here, to rectify the backwards build in `eat`
  mkDia = strokeLocTrail . (flip at) (pacmanStart p) . fromOffsets . reverse . pacmanVectors
  mergeSpawn = mconcat . map diagramPacman . pacmanSpawn
