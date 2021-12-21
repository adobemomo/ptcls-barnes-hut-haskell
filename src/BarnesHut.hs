module BarnesHut where

import DataStructs
import Debug.Trace

thetaThreshold :: Double
thetaThreshold = 1

--------------------------------------------------------------------------------
----------BH Algo---------------------------------------------------------------
--------------------------------------------------------------------------------
fromList :: [Particle] -> Vec -> Vec -> Tree
fromList [] tl br = emptyTree tl br
fromList (p : ps) tl br = calcSquard $ foldl (flip addParticle) (fromList ps tl br) [p]

toList :: Tree -> [Particle]
toList (Leaf Nothing _) = []
toList (Leaf (Just p) _) = [p]
toList (Tree t1 t2 t3 t4 _) = toList t1 ++ toList t2 ++ toList t3 ++ toList t4

-- is particle in squard
isInSquard :: Particle -> Squard -> Bool
isInSquard (Particle (Vec x y) _ _) (Squard (Vec cx cy) (Vec tlx tly) (Vec brx bry) _) =
  x >= tlx && x <= brx && y >= tly && y <= bry

-- add particle to tree
addParticle :: Particle -> Tree -> Tree
addParticle p (Leaf Nothing s)
  | p `isInSquard` s = Leaf {particle = Just p, squard = s}
  | otherwise = Leaf {particle = Nothing, squard = s}
addParticle p (Leaf (Just p') s)
  | p `isInSquard` s = addParticle p $ addParticle p' (emptyTree (topleft s) (bottomright s))
  | otherwise = Leaf (Just p') s
addParticle p (Tree t1 t2 t3 t4 s)
  | p `isInSquard` squard t1 = Tree (addParticle p t1) t2 t3 t4 s
  | p `isInSquard` squard t2 = Tree t1 (addParticle p t2) t3 t4 s
  | p `isInSquard` squard t3 = Tree t1 t2 (addParticle p t3) t4 s
  | p `isInSquard` squard t4 = Tree t1 t2 t3 (addParticle p t4) s
  | otherwise = Tree t1 t2 t3 t4 s

-- compute ntForce of particle2 on particle1 F=G*m1*m2/r^2
ntForce :: Particle -> Particle -> Double -> Vec
ntForce (Particle {coord = pos1, v = _, m = m1}) (Particle {coord = pos2, v = _, m = m2}) g = Vec (const * dx / r) (const * dy / r)
  where
    const = g * m1 * m2 / r
    r = dist pos1 pos2
    dx = x pos2 - x pos1
    dy = y pos2 - y pos1

-- compute acceleration of particle2 on particle1 a=F/m
acceleration :: Particle -> Particle -> Double -> Vec
acceleration p1 p2 g = ntForce p1 p2 g /. m p1

-- compute delta v of particle2 on  particle1 dv=a*dt
deltaV :: Particle -> Particle -> Double -> Double -> Vec
deltaV p1 p2 g dt | trace ("deltaV " ++ show p1 ++ " " ++ show p2 ++ " " ++ show (acceleration p1 p2 g *. dt)) False = undefined
deltaV p1 p2 g dt = acceleration p1 p2 g *. dt

-- update velocity of particle based on tree
updateV :: Particle -> Tree -> Double -> Double -> Particle
updateV p1 tree@(Tree _ _ _ _ s) g dt | trace ("theta=" ++ show (abs (distX (topleft s) (bottomright s)) / (dist (coord p1) (center s))) ++ " isC=" ++ show (abs ((distX (topleft s) (bottomright s)) / (dist (coord p1) (center s))) < thetaThreshold) ++ " p1.coord=" ++ show (coord p1) ++ " p1.v=" ++ show (v p1) ++ " tree.center=" ++ show (center s)) False = undefined
updateV p1 (Leaf (Just p2) _) g dt | trace ("p1==p2" ++ show (coord p1 == coord p2) ++ "p1.coord=" ++ show (coord p1) ++ " p2.coord" ++ show (coord p2) ++ " p1.v=" ++ show (v p1) ++ " dv=" ++ show (deltaV p1 p2 g dt)) False = undefined
updateV p (Leaf Nothing _) _ _ = p
updateV p1 (Leaf (Just p2) s) g dt
  | coord p1 == coord p2 = p1
  | otherwise = p1 {v = v p1 + deltaV p1 p2 g dt}
updateV p1 tree@(Tree _ _ _ _ s) g dt
  | isCongregate = (p1 {v = v p1 + deltaV p1 p2 g dt})
  | otherwise = foldTree (\t p -> updateV p t g dt) p1 tree
  where
    p2 = defaultParticle {coord = center s, m = mass s}
    r = dist (coord p1) (center s)
    squardSize = distX (topleft s) (bottomright s)
    theta = abs $ squardSize / r
    isCongregate = theta < thetaThreshold

-- update position of particle
updateP :: Particle -> Double -> Particle
updateP p dt = p {coord = coord p + v p *. dt}

-- update particle based on tree
updateParticle :: Tree -> Double -> Double -> Particle -> Particle
updateParticle tree g dt p = updateP (updateV p tree g dt) dt

-- calculate squard for tree
calcSquard :: Tree -> Tree
calcSquard (Leaf Nothing s) = Leaf Nothing s
calcSquard (Leaf (Just p) s) = Leaf (Just p) s {center = coord p, mass = m p}
calcSquard tree@(Tree t1 t2 t3 t4 s) = Tree t1' t2' t3' t4' s'
  where
    subtrees@[t1', t2', t3', t4'] = mapTree calcSquard tree
    s' = s {center = Vec newX newY, mass = totalMass}
    totalMass = foldr (\t acc -> acc + getMass t) 0 subtrees
    newX = foldr (\t acc -> acc + getCenterX t * getMass t) 0 subtrees / totalMass
    newY = foldr (\t acc -> acc + getCenterY t * getMass t) 0 subtrees / totalMass

--------------------------------------------------------------------------------
----------BH Algo---------------------------------------------------------------
--------------------------------------------------------------------------------

-- bh algorithm stimulation
bhstep :: Vec -> Vec -> Double -> Double -> [Particle] -> [Particle]
bhstep tl br g dt particles = particles'
  where
    tree = calcSquard $ fromList particles tl br
    particles' = map (updateParticle tree g dt) particles