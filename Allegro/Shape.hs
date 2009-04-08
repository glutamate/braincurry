{-# LANGUAGE DeriveDataTypeable #-}

module Allegro.Shape where

import Data.Typeable

instance Show (a->b) where show _ = "<function>"
instance Eq (a->b) where (==) = error "eq on function"

data Shape a = Polygon [(a,a)] (Colour a)
             | Rectangle (a,a) (a,a) (Colour a)
             | Ellipse (a,a) (a,a) a (Colour a)
             | Circle (a,a) a (Colour a)
             | Line (a,a) (a,a) (Colour a)
             | Text (a,a) a String (Colour a) -- a->String would be better. But how to define functor?
             | NoShape
               deriving (Eq, Show, Typeable)-- centre, radius

data Colour a = C a a a deriving (Eq, Show)

--instance Functor ((->)a) where
--    fmap = (.)

instance Functor Colour where
    fmap f (C r g b) = C (f r) (f g) (f b) 
           
instance Functor Shape where
    fmap f (Polygon pts col) = Polygon (map (apPair f) pts) (f `fmap` col)
    fmap f (Ellipse c rv e col) = Ellipse (apPair f c) (apPair f rv) (f e) (f `fmap` col)
    fmap f (Rectangle p1 p2 col) = Rectangle (apPair f p1) (apPair f p2) (f `fmap` col)
    fmap f (Line p1 p2 col) = Line (apPair f p1) (apPair f p2) (f `fmap` col)
    fmap f (Circle c r col) = Circle (apPair f c) (f r) (f `fmap` col)
    fmap f (Text pos sz txt col) = Text (apPair f pos) (f sz) (txt) (f `fmap` col) -- a->t, a->b, i want b->t 
    fmap f NoShape = NoShape

toCs :: Shape String -> String
toCs (Rectangle (x1,y1) (x2,y2) (C r g b)) 
    = concat ["draw_rect(scr, (int) ",x1,",(int) ",y1,", (int) ",x2,", (int) ",y2,", (int) ",r,", (int) ",g,", (int) ",b,");\n"]

setShapeColor (Polygon pts _) col = Polygon pts col
setShapeColor (Rectangle p1 p2 _) col = Rectangle p1 p2 col

apPair f (a,b) = (f a, f b)

resize z (Rectangle (x1,y1) (x2,y2) c) = Rectangle (xm-sx,ym-sy) (xm+sx,ym+sy) c
    where xm = (x2-x1)/2+x1
          ym = (y2-y1)/2+y1
          sx = (x2-x1)*z/2
          sy = (y2-y1)*z/2
resize z (Circle p radius c) = Circle p (radius*z) c
resize z (Ellipse ctr (ex, ey) e c) = Ellipse ctr (ex*z, ey*z) e c
resize z n@(NoShape) = n 

translate (x,y) (Rectangle (x1,y1) (x2,y2) c) = Rectangle (x1+x,y1+y) (x2+x,y2+y) c
translate (x,y) (Polygon pts c) = Polygon (map (\(x1,y1)->(x1+x,y1+y)) pts) c
translate (x,y) (Circle (cx,cy) radius c) = Circle (x+cx,y+cy) radius c
translate (x,y) (Ellipse (cx,cy) (ex, ey) e c) = Ellipse (x+cx,y+cy) (ex, ey) e c
translate (x,y) n@(NoShape) = n 

(x1, y1) @+ (x2, y2) = (x1+x2, y1+y2)
(x1, y1) @- (x2, y2) = (x1-x2, y1-y2)

pxs :: Fractional a => a
pxs = 1/240

nw, ne, se, sw :: Fractional a => (a,a)
nw = (-pxs, pxs)
ne = (pxs, pxs)
se = (pxs, -pxs)
sw = (-pxs, -pxs)

unitSqr :: Fractional a => Colour a -> Shape a
unitSqr c = (Rectangle (centre@+nw) (centre@+se) c) --640x480
unitCircle c = Circle centre pxs c

centre :: Num a => (a,a)
centre = (0,0)

green :: Num a => Colour a
green = C 0 255 0