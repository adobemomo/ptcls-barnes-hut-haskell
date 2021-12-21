module Main where

-- import Debug.Trace

import BarnesHut
import DataStructs

--------------------------------------------------------------------------------
----------Main------------------------------------------------------------------
--------------------------------------------------------------------------------
p = defaultParticle

-- pblack = p {m = 10000000000000000000000000}

-- p1 = p {coord = Vec 1 2, v = Vec 0.5 0.5, m = 3000000000}

-- p2 = p {coord = Vec (-3) (-5), v = Vec 0.5 0.5, m = 9000000000}

-- p3 = p {coord = Vec 2 (-2), v = Vec 0.5 0.5, m = 9000000000}

-- p4 = p {coord = Vec 4 (-4), v = Vec 0.5 0.5, m = 1000000000}

-- p5 = p {coord = Vec (-1) 2.6, v = zeroVec, m = 1000000000}

-- p6 = p {coord = Vec (-2) 4.5, v = zeroVec, m = 1000000000}

-- -- ps = [pblack, p1, p2, p3, p4, p5, p6]
-- pa = p {coord = Vec 1 0, v = Vec 0 0.1, m = 1000000000}

-- pb = p {coord = Vec (-1) 0, v = Vec 0 (-0.1), m = 1000000000}

-- ps = [pa, pb, p1, p2, p3, p4, p5, p6]

-- ps = [(x, y)]
ps = [p {coord = Vec x y, v = zeroVec, m = 1000000000} | x <- [-10 .. 10], y <- [-10 .. 10]]

tl = Vec (-10000) (-10000)

br = Vec 10000 10000

g = 6.67e-11

dt = 0.01

-- tree = calcSquard $ fromList ps tl br

-- emptytree = emptyTree tl br
--     ttt = calcSquard $ addParticle p6 $ addParticle p5 $ addParticle p4 $ addParticle p3 $ addParticle p2 $ addParticle p1 emptytree

-- loop :: Int -> ([Particle] -> Vec -> Vec -> Double -> Double -> [Particle]) -> [Particle] -> Vec -> Vec -> Double -> Double -> [Particle]
-- loop n f particles tl br g dt
--   | n == 0 = f particles tl br g dt
--   | otherwise = loop (n - 1) f (f particles tl br g dt) tl br g dt

main :: IO ()
main = do
  --   print $ loop iter bhstep particles tl br g dt
  -- let solutions = take 100 $ iterate (bhstep tl br g dt) ps
  -- let solutions = take 100 $ iterate (bhstepRpar tl br g dt) ps
  loop 0 ps
  where
    loop :: Int -> [Particle] -> IO ()
    loop 100 particles = do
      print particles
      return ()
    loop n particles = do
      loop (n + 1) $ bhstepParListChunk tl br g dt particles
      -- loop (n + 1) $ bhstepRpar tl br g dt particles
      return ()

-- print solutions

-- print $ tail $ (fmap . fmap) coord solutions

-- where
--   iter = 5
--   tl = Vec (-100) (-100)
--   br = Vec 100 100
--   g = 6.67e-11
--   dt = 0.0001
--   particles = ps

-- :l final/src/main.hs