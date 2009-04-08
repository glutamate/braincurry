{-# LANGUAGE FlexibleInstances #-}

module Allegro.Expr2 where

import Data.List

data Declare = MkShape (Shape3 NExpr)
             | IfD BExpr Declare Declare
	     | LetN String NExpr
             | Empty
             | LogN String NExpr
	     | ConstShape Int (Shape3 Double)
		deriving (Show, Eq)

data Vector a = Vec a a a deriving (Show, Eq)
data Colour a = Col a a a deriving (Show, Eq)

data Shape3 a = Box (Vector a)
       	      | Sphere a
              | Translate (Vector a) (Shape3 a)
              | Rotate a a a (Shape3 a)
	      | WithColour (Colour a) (Shape3 a)
              | IfS BExpr (Shape3 a) (Shape3 a)
              | Shapes [Shape3 a]
		deriving (Show, Eq)

data NFun = NFun {
      cName :: String,
      showInfix :: Bool,
      hsFun :: [Double]->Double
    }
{-
class PP a where
    pp :: Int -> a-> String

instance PP Declare where
    pp ind (If b dc da) = concat [spaces ind, 
                                  "if(", pp 0 b ,") {\n", spaces (ind+3) ]

-}

spaces n = replicate n ' '  

instance Show NFun where
    show (NFun n _ _) = "NFun "++n

instance Eq NFun where
    (NFun n _ _) == (NFun n' _ _) = n==n'

data NExpr = Const Double
           | NumF NFun [NExpr]
           | Time
	   | VarN String
           | IfN BExpr NExpr NExpr
		deriving (Show, Eq)

isConstN (Const _) = True
isConstN (NumF nf es) = and $ map isConstN es 
isConstN (IfN p n1 n2) = isConstB p && isConstN n1 && isConstN n2
isConstN _ = False

data BExpr = T | F 
           | And BExpr BExpr | Or BExpr BExpr | Not BExpr 
           | GTN NExpr NExpr | LTN NExpr NExpr
		deriving (Show, Eq)

isConstB T = True
isConstB F = True
isConstB (And b1 b2) = isConstB b1 && isConstB b2
isConstB (Or b1 b2) = isConstB b1 && isConstB b2
isConstB (Not b) = isConstB b
isConstB (GTN n1 n2) = isConstN n1 && isConstN n2
isConstB (LTN n1 n2) = isConstN n1 && isConstN n2
-- isConstB _ = False

staticEvalN :: NExpr -> Double
staticEvalN (Const n) = n
staticEvalN (IfN p c a) = if staticEvalB p
                            then staticEvalN c
                            else staticEvalN a
staticEvalN (NumF (NFun _ _ hsFun) nes) = hsFun $ map staticEvalN nes

staticEvalB :: BExpr -> Bool
staticEvalB T =True
staticEvalB F =False
staticEvalB (And b1 b2) = staticEvalB b1 && staticEvalB b2 
staticEvalB (Or b1 b2) = staticEvalB b1 || staticEvalB b2 
staticEvalB (Not b1) = not (staticEvalB b1)
staticEvalB (GTN n1 n2) = staticEvalN n1 > staticEvalN n2
staticEvalB (LTN n1 n2) = staticEvalN n1 < staticEvalN n2

class NExprMap e where
    nexprMap :: (NExpr->NExpr)->e->e

instance NExprMap Declare where
    nexprMap nef (MkShape s) = MkShape $ fmap nef s
    nexprMap nef (IfD p c a) = IfD (nexprMap nef p) (nexprMap nef c) (nexprMap nef a)
    nexprMap nef (LetN s ne) = LetN s (nexprMap nef ne)
    nexprMap nef (LogN s ne) = LetN s (nexprMap nef ne)
    nexprMap _ de = de

instance NExprMap BExpr where
    nexprMap nef (And b1 b2) = And (nexprMap nef b1) (nexprMap nef b2)
    nexprMap nef (Or b1 b2) = Or (nexprMap nef b1) (nexprMap nef b2)
    nexprMap nef (Not b) = Not (nexprMap nef b) 

    nexprMap nef (GTN n1 n2) = GTN (nexprMap nef n1) (nexprMap nef n2)
    nexprMap nef (LTN n1 n2) = LTN (nexprMap nef n1) (nexprMap nef n2)

    nexprMap _ be = be

instance NExprMap NExpr where
    nexprMap nef ne = nef (nexprMap' nef ne)
        where nexprMap' nf (IfN p c a) = IfN (nexprMap nf p) (nexprMap nf c) (nexprMap nf a)
              nexprMap' nf (NumF nfun nes) =  NumF nfun $ map (nexprMap nf) nes

partiallyEval :: Declare->Declare
partiallyEval dec = nexprMap peNExpr dec
    where peNExpr n | isConstN n = Const $ staticEvalN n
                    | otherwise = n

--http://www.allegro.cc/forums/thread/592722
compile :: [Declare] -> String
compile decls = unlines ["void the_animation(double time) {",
                         "glMatrixMode( GL_PROJECTION );",
                         "glLoadIdentity();",
                         "glFrustum(-0.2, 0.2, -0.15, 0.15, 0.163, 100.0);",
                         "glMatrixMode( GL_MODELVIEW );",
                         "glLoadIdentity();",
                         -- "gluPerspective( 80.2, 640.0/480.0, 0.163, 100.0);",
                         concatMap toC decls,
                         "}"]

maxN e1 e2 = NumF (NFun "max" False (\(x:y:_)->max x y)) [e1,e2]
minN e1 e2 = NumF (NFun "min" False (\(x:y:_)->min x y)) [e1,e2]

loom lov = let l = 0.298
               v = l/(lov*2) in
           [LetN "distance" $ minN (v*(Time-5)) (-0.17),
            -- LogN "dist" $ VarN "distance",
            -- LogN "dist_nomax" $ VarN "distance",
            MkShape (WithColour black . Translate (Vec (0) 0 (VarN "distance")) $ centreCube l)]

loomDisplaced d lov = let l = 0.298
                          v = l/(lov*2) in
                      [LetN "tp" $ minN Time 5,
                       LetN "distance" $ v*(VarN "tp"-5),
                       MkShape (WithColour black . Translate (Vec (d*(VarN "tp")/5) 0 (VarN "distance")) $ centreCube l)]

red, green, blue, white, black :: Num a => Colour a
red = Col 1 0 0
green = Col 0 1 0
blue = Col 0 0 1
white = Col 1 1 1
black = Col 0 0 0

centreCube l = Translate (Vec (-l/2) (-l/2) 0) $ Box (Vec l l l)

--loom l v = [MkShape $ Translate (Vec 0 0 (5-Time)) $ Box (Vec l l l)]

class ToC a where 
	toC :: a-> String

instance ToC a => ToC [a] where
	toC [] = "" -- concatMap toC
	toC (e:es) = toC e ++ toC es

(+^) :: ToC a => String -> a -> String
a +^ b = a++(toC b)

inParens s = "("++s++")"

inParToC :: ToC a => a -> String
inParToC = inParens . toC

-- instance ToC String where toC = id

instance ToC Declare where
	toC (IfD p c a) = concat ["if(" ,toC p , ") {\n" , toC c, "\n }else{\n" , toC a , "}\n"]
	toC (LetN n e) = "double "++n++" = "+^e++";\n"
	toC (LogN n e) = "printf(\"at time %g, "++n++" = %g\\n\", time, "+^e++");\n"
--http://www.3dbuzz.com/vbforum/showthread.php?t=118279 for sphere
	toC (MkShape s) = "glPushMatrix();\n"+^s++"glPopMatrix();\n"
	toC (Empty) = ""
		
instance ToC (Shape3 NExpr) where 
--http://www.morrowland.com/apron/tutorials/gl/gl_rotating_cube.php
	toC (Box v) = concat ["glBegin(GL_QUADS);\n",toC (vecToQuad v Z)
					  ,concatMap (toC . (+(v//X//Y))) $ vecToQuad v Z
					  ,toC (vecToQuad v X)
					  ,concatMap (toC . (+(v//Z//Y))) $ vecToQuad v X
					  ,toC (vecToQuad v Y)
					  ,concatMap (toC . (+(v//Z//X))) $ vecToQuad v Y
		   		          ,"\nglEnd();\n"]
	toC (WithColour col s) = toC col ++ toC s
	toC (Translate (Vec x y z) s) = "glTranslated("+^x++", "+^y++", "+^z++");\n"+^s

-- glTranslatef(0.0f, 0.0f,-7.0f);

-- +^(vflatIn Z $ flatIn X v)+^(flatIn Z v)+^(flatIn X $ flatIn Z v)


vecToQuad v Z = [v//X//Z, v//Z, v//Y//Z, origin]
vecToQuad v X = [v//Y//X, v//X, v//Z//X, origin]
vecToQuad v Y = [v//Z//Y, v//Y, v//X//Y, origin]

infixl 5 //
v // dim = flatIn dim v

instance ToC (Vector NExpr) where
	toC (Vec x y z) = concat ["glVertex3d(", toC x, ",", toC y, ",",toC z, ");\n"]

instance ToC (Colour NExpr) where --glColor3f(0.0f,1.0f,0.0f);
	toC (Col x y z) = concat ["glColor3d(", toC x, ",", toC y, ",",toC z, ");\n"]

instance ToC BExpr where
	toC T = "true"
	toC F = "false"
	toC (And b1 b2) = inParToC b1 ++"&&"++ inParToC b2
	toC (Or b1 b2) = "("+^ b1 ++") || ("+^ b2 ++")"
	toC (Not b1) = "!("+^ b1++")"
	toC (GTN n1 n2) = "("+^ n1++")>("+^ n2++")"

instance ToC NExpr where
	toC Time ="time"
	toC (Const n) = show n
	toC (NumF (NFun f False _) es) = f++inParens(intercalate ", " $ map toC es)
	toC (NumF (NFun f True _) [e1, e2]) = inParToC e1 ++ f ++ inParToC e2	
	toC (IfN p c a) = inParToC p ++ "?"++inParToC c ++ ":"++inParToC a
	toC (VarN s) =s

instance Functor Shape3 where
	fmap f (Box v) = Box (fmap f v)
	fmap f (Sphere r) = Sphere (f r)
        fmap f (Translate v s) = Translate (fmap f v) (fmap f s)
        fmap f (Rotate rx ry rz s) = Rotate (f rx) (f ry) (f rz) (fmap f s)
        fmap f (IfS p c a) = IfS p (fmap f c) (fmap f a)
        fmap f (Shapes ss) = Shapes $ map (fmap f) ss 

instance Functor Vector where 
	fmap f (Vec x y z) = Vec (f x) (f y) (f z)

instance Functor Colour where 
	fmap f (Col x y z) = Col (f x) (f y) (f z)

instance Num NExpr where
    (Const n1) + (Const n2) = Const (n1+n2)
    e1 + e2 = NumF (NFun "+" True (\(x:y:_)->x+y)) [e1,e2]
    (Const n1) - (Const n2) = Const (n1-n2)
    e1 - e2 = NumF (NFun "-" True (\(x:y:_)->x-y)) [e1,e2]
    (Const n1) * (Const n2) = Const (n1*n2)
    e1 * e2 = NumF (NFun "*" True (\(x:y:_)->x*y)) [e1,e2]
    abs (Const n) = Const (abs n)
    abs e = NumF (NFun "abs" False (\(x:_)->abs x)) [e]
    signum e = NumF (NFun "signum" False (\(x:_)->signum x)) [e]
    fromInteger = Const . realToFrac

instance Fractional NExpr where
    e1 / e2 = NumF (NFun "/" True (\(x:y:_)->x/y)) [e1,e2]
    fromRational = Const . realToFrac

instance Num a => Num (Vector a) where
    (Vec x1 y1 z1) + (Vec x2 y2 z2) = Vec (x1+x2) (y1+y2) (z1+z2)
    (Vec x1 y1 z1) - (Vec x2 y2 z2) = Vec (x1-x2) (y1-y2) (z1-z2)
    (Vec x1 y1 z1) * (Vec x2 y2 z2) = Vec (x1*x2) (y1*y2) (z1*z2)
    abs v = fmap abs v
    signum v = fmap signum v
    fromInteger i = Vec (fromInteger i) (fromInteger i) (fromInteger i)

origin :: Num a => Vector a
origin = Vec 0 0 0

data Dim = X | Y | Z
dims = [X, Y, Z]

flatIn :: Num a => Dim -> Vector a -> Vector a
flatIn X (Vec _ y z) = Vec 0 y z
flatIn Y (Vec x _ z) = Vec x 0 z
flatIn Z (Vec x y _) = Vec x y 0

