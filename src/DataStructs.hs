module DataStructs where

import Control.DeepSeq
import Debug.Trace

--------------------------------------------------------------------------------
----------Vec-------------------------------------------------------------------
--------------------------------------------------------------------------------

data Vec = Vec {x :: Double, y :: Double} deriving (Eq)

instance Show Vec where
  show (Vec x y) = "[" ++ show x ++ "," ++ show y ++ "]"

zeroVec = Vec 0 0

dist :: Vec -> Vec -> Double
dist (Vec x1 y1) (Vec x2 y2) = sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2

distX :: Vec -> Vec -> Double
distX (Vec x1 y1) (Vec x2 y2) = x1 - x2

scalar :: Vec -> Double
scalar v = dist v zeroVec

instance Num Vec where
  (+) (Vec a b) (Vec c d) = Vec (a + c) (b + d)
  (-) (Vec a b) (Vec c d) = Vec (a - c) (b - d)
  (*) (Vec a b) (Vec c d) = Vec (a * c) (b * d)
  abs (Vec a b) = Vec (abs a) (abs b)
  signum (Vec a b) = Vec (signum a) (signum b)
  fromInteger a = Vec (fromInteger a :: Double) (fromInteger a :: Double)

instance NFData Vec where
  rnf (Vec a b) = rnf a `deepseq` rnf b

(*.) :: Vec -> Double -> Vec
(Vec x y) *. n = Vec (x * n) (y * n)

(/.) :: Vec -> Double -> Vec
(Vec x y) /. n = Vec (x / n) (y / n)

--------------------------------------------------------------------------------
----------Particle--------------------------------------------------------------
--------------------------------------------------------------------------------

data Particle = Particle {coord :: Vec, v :: Vec, m :: Double} deriving (Eq)

instance Show Particle where
  show (Particle c v m) = "[P] pos=" ++ show c ++ ", v=" ++ show v ++ ", m=" ++ show m

instance NFData Particle where
  rnf (Particle c v m) = rnf c `deepseq` rnf v `deepseq` rnf m

defaultParticle = Particle zeroVec zeroVec 1

--------------------------------------------------------------------------------
----------Squard----------------------------------------------------------------
--------------------------------------------------------------------------------

data Squard = Squard {center :: Vec, topleft :: Vec, bottomright :: Vec, mass :: Double} deriving (Eq)

instance Show Squard where
  show (Squard c tl br m) = "[S] center=(" ++ show (x c) ++ "," ++ show (y c) ++ "), size=" ++ show (abs (distX tl br)) ++ ", mass=" ++ show m

instance NFData Squard where
  rnf (Squard c tl br m) = rnf c `deepseq` rnf tl `deepseq` rnf br `deepseq` rnf m

--------------------------------------------------------------------------------
----------Tree------------------------------------------------------------------
--------------------------------------------------------------------------------

data Tree
  = Tree {subtree1 :: Tree, subtree2 :: Tree, subtree3 :: Tree, subtree4 :: Tree, squard :: Squard}
  | Leaf {particle :: Maybe Particle, squard :: Squard}
  deriving (Eq)

instance NFData Tree where
  rnf (Tree a b c d e) = rnf a `deepseq` rnf b `deepseq` rnf c `deepseq` rnf d `deepseq` rnf e
  rnf (Leaf (Just a) b) = rnf a `deepseq` rnf b
  rnf (Leaf Nothing b) = rnf b

mapTree :: (Tree -> a) -> Tree -> [a]
mapTree f (Tree a b c d _) = [f a, f b, f c, f d]
mapTree f leaf@Leaf {} = [f leaf]

foldTree :: (Tree -> a -> a) -> a -> Tree -> a
-- foldTree f acc (Tree a b c d _) | trace ("foldTree: " ++ show (center (squard a)) ++ " " ++ show (center (squard b)) ++ " " ++ show (center (squard c)) ++ " " ++ show (center (squard d))) False = undefined
foldTree f acc (Tree t1 t2 t3 t4 _) = foldTree f (foldTree f (foldTree f (foldTree f acc t1) t2) t3) t4
foldTree f acc leaf@Leaf {} = f leaf acc

printTree :: Tree -> Int -> String
printTree tree@(Tree _ _ _ _ s) level = concat $ sq : branches
  where
    sq = (if level /= 0 then "\\_" else "") ++ show s
    branches = mapTree (\t -> "\n" ++ replicate level '-' ++ printTree t (level + 1)) tree
printTree leaf@Leaf {} _ = "\\_" ++ show leaf

instance Show Tree where
  show tree@(Tree {}) = printTree tree 0
  show (Leaf p s) = show p ++ " " ++ show s

getMass :: Tree -> Double
getMass (Tree _ _ _ _ s) = mass s
getMass (Leaf (Just p) _) = m p
getMass (Leaf Nothing _) = 0

getCenter :: Tree -> Vec
getCenter (Tree _ _ _ _ s) = center s
getCenter (Leaf (Just p) _) = coord p
getCenter (Leaf Nothing _) = zeroVec

getCenterX :: Tree -> Double
getCenterX tree = x $ getCenter tree

getCenterY :: Tree -> Double
getCenterY tree = y $ getCenter tree

emptyLeaf :: Vec -> Vec -> Tree
emptyLeaf topleft bottomright = Leaf {particle = Nothing, squard = Squard {center = Vec 0 0, topleft = topleft, bottomright = bottomright, mass = 0}}

emptyTree :: Vec -> Vec -> Tree
emptyTree topleft bottomright =
  Tree
    (emptyLeaf topleft (Vec xmid ymid))
    (emptyLeaf (Vec xmid ymid) bottomright)
    (emptyLeaf (Vec xmid ymin) (Vec xmax ymid))
    (emptyLeaf (Vec xmin ymid) (Vec xmid ymax))
    (Squard {center = Vec xmid ymid, topleft = topleft, bottomright = bottomright, mass = 0})
  where
    xmin = x topleft
    xmax = x bottomright
    ymin = y topleft
    ymax = y bottomright
    xmid = (xmin + xmax) / 2
    ymid = (ymin + ymax) / 2
