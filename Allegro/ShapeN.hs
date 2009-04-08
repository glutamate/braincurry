{-# LANGUAGE FlexibleInstances #-}

module Allegro.ShapeN where

import Data.List
import Data.Ord 
import Allegro.Vector

-- Colours

data Colour a = Col a a a deriving (Show, Eq)

instance Functor Colour where 
	fmap f (Col x y z) = Col (f x) (f y) (f z)


-- Shapes

data Shape d a = Dot
               | LineSegment (Vector d a)
               | Sphere a
               | Box (Vector d a)
               | Polygon [Vector d a]
               | Text String [TextAttr a]
               | Translate (Vector d a) (Shape d a)
               | Rotate (Vector d Double) (Shape d a)
               | Scale (Vector d a) (Shape d a)
               | LineThickness a (Shape d a)
               | LineColour (Colour a) (Shape d a)
               | FillColour (Colour a) (Shape d a)
               | Shapes [Shape d a]
                 deriving Show

extent :: (TInt d, Num a, Ord a) => Shape d a -> (Vector d a,Vector d a)
extent Dot = (0,0)
extent (LineSegment (Vec vl)) = (Vec $ map (min 0) vl,Vec $ map (max 0) vl)
extent (Sphere r) = (constVec (-1*r),constVec (r))
extent (Box v) = (0,v)
extent (Translate v shp) = let (on, op) = extent shp in
                         (on+v, op+v)
extent (Scale (Vec v) shp) = let (Vec on, Vec op) = extent shp in
                       (Vec $ zipWith (*) v on, Vec $ zipWith (*) v op )
extent (Shapes shps) = foldl1 op $  map extent shps
    where op (Vec on, Vec op) (Vec nn,Vec np) = (Vec $ zipWith (min) on nn, Vec $ zipWith (max) op np)
extent (LineColour _ shp) = extent shp
extent (FillColour _ shp) = extent shp
extent (LineThickness _ shp) = extent shp


data TextAttr a = FontName String
                | FontSize a
                | Bold | Italic | Underline
                  deriving Show

instance Functor TextAttr where
        fmap f (FontSize x) = (FontSize $ f x)
        fmap f (FontName s) = (FontName s)
        fmap f Bold = Bold
        fmap f Italic = Italic
        fmap f Underline = Underline

instance Functor (Shape d) where
        fmap f Dot = Dot
	fmap f (Box v) = Box (fmap f v)
	fmap f (Sphere r) = Sphere (f r)
        fmap f (Translate v s) = Translate (fmap f v) (fmap f s)
        fmap f (Rotate v s) = Rotate (v) (fmap f s)
        fmap f (LineSegment v) = LineSegment (fmap f v)
        fmap f (Polygon pts) = Polygon $ map (fmap f) pts
        fmap f (Text t ats) = Text t $ map (fmap f) ats
        fmap f (LineColour c s) = LineColour (fmap f c) (fmap f s)
        fmap f (FillColour c s) = FillColour (fmap f c) (fmap f s)
        fmap f (LineThickness thk s) = LineThickness (f thk) (fmap f s) 

-- transform sizes and colour separately
sizeColMap :: (a->b) -> (a->b) -> Shape d a -> Shape d b
sizeColMap sf cf Dot = Dot
sizeColMap sf cf (Box v) = Box (fmap sf v)
sizeColMap sf cf (Sphere r) = Sphere (sf r)
sizeColMap sf cf (Translate v s) = Translate (fmap sf v) (sizeColMap sf cf s)
sizeColMap sf cf (Rotate v s) = Rotate (v) (sizeColMap sf cf s)
sizeColMap sf cf (LineSegment v) = LineSegment (fmap sf v)
sizeColMap sf cf (Polygon pts) = Polygon $ map (fmap sf) pts
sizeColMap sf cf (Text t ats) = Text t $ map (fmap sf) ats
sizeColMap sf cf (LineColour c s) = LineColour (fmap cf c) (sizeColMap sf cf s)
sizeColMap sf cf (FillColour c s) = FillColour (fmap cf c) (sizeColMap sf cf s)
sizeColMap sf cf (LineThickness thk s) = LineThickness (sf thk) (sizeColMap sf cf s) 

mapSuccPairs :: (a->a->b) -> [a] -> [b]
mapSuccPairs f (x1:l@(x2:_)) = f x1 x2 : mapSuccPairs f l
mapSuccPairs f _ = []

plotLstVals :: [Double] -> [Double] -> [Shape Two Double]
plotLstVals yvls xvls = let yPts = mapSuccPairs (,) yvls
                            xPts = mapSuccPairs (,) xvls in
                        zipWith mkLine yPts xPts 
    where mkLine (y1, y2) (x1, x2) = let v1 = (vec2 x1 y1) in
                                     Translate  v1 $ LineSegment (vec2 x2 y2 - v1)

