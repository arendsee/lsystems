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
    pacmanStart :: P3 Double
  , pacmanEnd :: P3 Double
  , pacmanAngle :: (Double, Double, Double)
  , pacmanWidth :: Double
  , pacmanSpawn :: [Pacman]
  , pacmanVectors :: [V2 Double]
}

pacman0 :: Pacman
pacman0 = Pacman {
      pacmanStart   = p3 (0,0,0) :: P3 Double
    , pacmanEnd     = p3 (0,0,0) :: P3 Double
    , pacmanAngle   = (90,0,0)
    , pacmanWidth   = 1.0
    , pacmanSpawn   = []
    , pacmanVectors = []
  }

spawnPacman :: Pacman -> Pacman
spawnPacman p = Pacman {
      pacmanStart   = pacmanEnd p
    , pacmanEnd     = pacmanEnd p
    , pacmanAngle   = pacmanAngle p
    , pacmanWidth   = pacmanWidth p
    , pacmanSpawn   = []
    , pacmanVectors = []
  }

eat :: Pacman -> Node -> Pacman 
eat t (NodeRotate _ x1 y1 z1) = case (t pacmanAngle) of
  (x2,y2,z2) -> t { pacmanAngle = (x1+x2, y1+y2, z1+z2) }
eat t (NodeDraw _ x) = t {
      pacmanEnd = pacmanEnd t # translate v' # sized (mkWidth $ pacmanWidth t)
      -- build backwards for performance reasons, this will need to be reversed later
    , pacmanVectors = v' : (pacmanVectors t)
  } where
    -- TODO: this needs to be projected on the XY plain
    v' = e (pacmanAngle t) # scale x

eat t (NodeDummy _ _) = t
eat t (NodeWidth _ x) = t { pacmanWidth = x }
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
