module Main where

-- import Debug.Trace

import BarnesHut
import DataStructs

--------------------------------------------------------------------------------
----------Main------------------------------------------------------------------
--------------------------------------------------------------------------------
p = defaultParticle

pblack = p {m = 10000000000000000000000000}

p1 = p {coord = Vec 1 2, v = Vec 0.5 0.5, m = 3}

p2 = p {coord = Vec (-3) (-5), v = Vec 0.5 0.5, m = 9}

p3 = p {coord = Vec 2 (-2), v = Vec 0.5 0.5, m = 9}

p4 = p {coord = Vec 4 (-4), v = Vec 0.5 0.5, m = 1}

p5 = p {coord = Vec (-1) 2.6, v = zeroVec, m = 1}

p6 = p {coord = Vec (-2) 4.5, v = zeroVec, m = 1}

-- ps = [pblack, p1, p2, p3, p4, p5, p6]
pa = p {coord = Vec 1 (-1), v = Vec 0 0.1, m = 1000000}

pb = p {coord = Vec (-1) 1, v = Vec 0.1 0, m = 1000000}

ps = [pa, pb]

tl = Vec (-100) (-100)

br = Vec 100 100

tree = calcSquard $ fromList ps tl br

-- emptytree = emptyTree tl br
--     ttt = calcSquard $ addParticle p6 $ addParticle p5 $ addParticle p4 $ addParticle p3 $ addParticle p2 $ addParticle p1 emptytree

loop :: Int -> ([Particle] -> Vec -> Vec -> Double -> Double -> [Particle]) -> [Particle] -> Vec -> Vec -> Double -> Double -> [Particle]
loop n f particles tl br g dt
  | n == 0 = f particles tl br g dt
  | otherwise = loop (n - 1) f (f particles tl br g dt) tl br g dt

main :: IO ()
main = do
  --   print $ loop iter bhstep particles tl br g dt
  let solutions = take 100 $ iterate (bhstep tl br g dt) particles
  -- print solutions
  print $ (fmap . fmap) coord solutions
  where
    iter = 5
    tl = Vec (-100) (-100)
    br = Vec 100 100
    g = 6.67e-11
    dt = 0.1
    particles = ps

-- :l final/src/main.hs