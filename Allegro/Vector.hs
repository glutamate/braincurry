{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}

module Allegro.Vector where

import Data.List
import Data.Ord 
import Data.Typeable

-- Typed Integers

data Z = Z deriving (Show, Typeable)
data S a = S a deriving (Show, Typeable)
data Neg a = Neg a deriving (Show, Typeable)

type Zero = Z
type One = S Zero
type Two = S One
type Three = S Two
type Four = S Three

class TInt t where
    toInt :: t -> Int
             
instance TInt Z where
    toInt _ = 0

instance TInt n => TInt (S n) where
    toInt = (+1) . toInt . prec 

prec :: S a -> a
prec = undefined

-- Vectors

data Vector d a = Vec [a] deriving (Show, Eq, Typeable)

instance Functor (Vector d) where 
	fmap f (Vec pts) = Vec $ map f pts

(*^) :: Num a => a -> Vector d a -> Vector d a
s *^ (Vec pts) = Vec $ map (*s) pts

ndims :: TInt d => Vector d a -> Int
ndims = toInt . tlen
    where tlen :: Vector d a -> d
          tlen = undefined

len :: Floating a => Vector d a -> a
len (Vec pts) = sqrt . sum . map (**2) $ pts

vecToList :: Vector d a->[a]
vecToList (Vec pts) = pts

instance (Num a, TInt d) => Num (Vector d a) where
    (Vec ps1) + (Vec ps2) = Vec (zipWith (+) ps1 ps2)
    (Vec ps1) - (Vec ps2) = Vec (zipWith (-) ps1 ps2)
    (Vec ps1) * (Vec ps2) = Vec (zipWith (*) ps1 ps2) --no
    abs v = fmap abs v
    signum v = fmap signum v
    fromInteger i = v
        where v = Vec (replicate (ndims v) (fromInteger i))

instance (Fractional a, TInt d) => Fractional (Vector d a) where
    (Vec ps1) / (Vec ps2) = Vec (zipWith (/) ps1 ps2) --no
    fromRational x = v
        where v = Vec (replicate (ndims v) (fromRational x))

constVec :: TInt d =>a -> Vector d a
constVec vl = v
        where v = Vec (replicate (ndims v) (vl))

vec2 :: a ->  a -> Vector Two a
vec2 x y = Vec [x, y]

vec3 ::  a ->  a ->  a -> Vector Three a
vec3 x y z = Vec [x, y, z]

vec4 ::  a ->  a ->  a -> a -> Vector Four a
vec4 x y z w = Vec [x, y, z, w]

centroid :: (Fractional a, TInt d) => Vector d a -> [Vector d a] -> Vector d a
centroid w pts = recip (realToFrac $ length pts) *^ sum (map (w*) pts) 

--distToEach :: Vector d a -> [Vector d a] -> [a]
                   
groupByNearest :: (Floating a, Eq a, TInt d, Ord a) => Vector d a -> [Vector d a] -> [Vector d a] -> [[Vector d a]]
groupByNearest w pts clusters 
    = groupBy (equating (snd . nearestCluster)) pts
    where nearestCluster p = minimumBy (comparing (\(centroid, indx) ->len ((centroid-p)*w))) clWithIndex
          clWithIndex = zip clusters [1..]

--[a] -> [b] -> (a->[b]->Int) -> [Int]

boundingBox :: Ord a => [Vector d a] -> (Vector d a, Vector d a)
boundingBox (v:vcts) = boundingBox' vcts (v,v) 
    where boundingBox' [] bb = bb
          boundingBox' ((Vec pts):vs) (Vec minpts, Vec maxpts) 
              = boundingBox' vs (Vec $ zipWith min minpts pts,
                                 Vec $ zipWith max maxpts pts)

kMeans :: (Floating a, Eq a, TInt d, Ord a) =>  Vector d a -> [Vector d a]-> [Vector d a] -> [[Vector d a]]
kMeans weights pts initCtroids = iterate (updateCentroids weights pts) initCtroids

updateCentroids :: (Floating a, Eq a, TInt d, Ord a) => Vector d a -> [Vector d a] -> [Vector d a] -> [Vector d a]
updateCentroids weights pts ctroids = map (centroid weights) $ groupByNearest weights pts ctroids

-- Matrices

data Matrix d1 d2 a = Mat [[a]] deriving (Show, Eq)

instance Functor (Matrix d1 d2) where 
	fmap f (Mat pts) = Mat $ map (map f) pts


--from Cabal 
equating :: Eq a => (b -> a) -> b -> b -> Bool
equating p x y = p x == p y
