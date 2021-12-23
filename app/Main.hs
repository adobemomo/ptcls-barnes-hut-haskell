module Main where

import BarnesHut
import DataStructs

p :: Particle
p = defaultParticle

ps :: [Particle]
ps = [p {coord = Vec x' y', v = zeroVec, m = 1000000000} | x' <- [-10 .. 10], y' <- [-10 .. 10]]

tl :: Vec
tl = Vec (-10) (-10)

br :: Vec
br = Vec 10 10

g :: Double
g = 6.67e-11

dt :: Double
dt = 0.01

main :: IO ()
main = do
  loop 0 ps
  where
    loop :: Int -> [Particle] -> IO ()
    loop 1000 particles = do
      print particles
      return ()
    loop n particles = do
      loop (n + 1) $ bhstepParListChunk tl br g dt particles
      -- loop (n + 1) $ bhstepRpar tl br g dt particles
      return ()