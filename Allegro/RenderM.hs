{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, TypeSynonymInstances #-}

module Allegro.RenderM where

import Allegro.Vector
import qualified Graphics.Rendering.OpenGL as OGL
import qualified Graphics.UI.GLUT as G
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.Matrix as CM

data Colour a = Col a a a deriving (Show, Eq)

class (Monad m, TInt d) => RenderM m d a | m -> d a where
    translate :: Vector d a -> m () -> m ()
    scale :: Vector d a -> m () -> m ()
    rotate :: a -> Vector d a -> m () -> m ()
    box :: Vector d a -> m ()
    fillCol :: Colour a -> m () -> m ()

instance RenderM IO Three Double where 
    box (Vec (x:y:z:_)) = OGL.renderPrimitive OGL.Quads $ do
                           OGL.vertex $ OGL.Vertex3 (0::Double) 0 0
                           OGL.vertex $ OGL.Vertex3 x 0 0
                           OGL.vertex $ OGL.Vertex3 x y 0
                           OGL.vertex $ OGL.Vertex3 0 y 0
    translate (Vec (x:y:z:_)) s = OGL.preservingMatrix $ do
                                    OGL.translate $ OGL.Vector3 x y z
                                    s
    fillCol (Col r g b) s = OGL.preservingMatrix $ do
                                    OGL.color $ OGL.Color3 r g b
                                    s
    scale (Vec (x:y:z:_)) s = OGL.preservingMatrix $ do
                                    OGL.scale x y z
                                    s
    rotate deg (Vec (x:y:z:_)) s = OGL.preservingMatrix $ do
                                     OGL.rotate deg $ OGL.Vector3 x y z
                                     s