module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import qualified Shape as S
import Control.Concurrent.STM
import Expr
import System.Mem

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = map (\k -> (sin(2*pi*k/12),cos(2*pi*k/12),0.0)) [1..12]

readTV = atomically . readTVar
writeTV vr vl = atomically $ writeTVar vr vl

main = mkAnim 5 []

mkAnim :: Int -> [Double]->IO ()
mkAnim 0 wcl = do putStrLn "----"
                  print $ (sum wcl/(realToFrac $ length wcl))*1000
mkAnim n wstCaseLst = do 
  (progname,_) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  createWindow "Hello World"
  reshapeCallback $= Just reshape
  actionOnWindowClose $= ContinueExectuion
  --fullScreen
  stime <- get elapsedTime

  anim <- atomically $ newTVar (Just (myEval $ loom1 0.1))
  curTime <- atomically $ newTVar stime 
  startTime <- atomically $ newTVar stime
  frameStats <- atomically $ newTVar (0,0,0,-1.1)
 
  displayCallback $= (display anim curTime frameStats)
  idleCallback $= Just (idle startTime curTime)
v v v v v v v
  --keyboardMouseCallback $= Just (keyboardMouse frameStats)

  mainLoop
keyboardMouse fs key state modifiers position = readTV fs >>= print
*************
--  keyboardMouseCallback $= Just (keyboardMouse frameStats)  performGC
  mainLoop
  (avg, nfr, mx, lst) <- readTV frameStats
  print (avg*1000, nfr, mx*1000)
  mkAnim (n-(1::Int)) (mx:wstCaseLst)

--keyboardMouse fs key state modifiers position = readTV fs >>= print
^ ^ ^ ^ ^ ^ ^

--drawShape s = drawShape' (realToFrac `fmap` s)
--    where drawShape' :: Shape GLfloat -> IO ()

drawGreenCtrSquare :: GLfloat -> GLfloat ->IO ()
drawGreenCtrSquare s g
   = preservingMatrix $ do
        color $ Color3 0 g 0
        renderPrimitive Quads $ do
                           vertex $ Vertex3 s s 0
                           vertex $ Vertex3 (-s) s 0
                           vertex $ Vertex3 (-s) (-s) 0
                           vertex $ Vertex3 s (-s) 0

drawShape :: S.Shape Double -> IO ()
drawShape (S.Rectangle (x1, y1) (x2, y2) (S.C r g b))
    = preservingMatrix $ do
        color $ Color3 r g b
        renderPrimitive Quads $ do
                           vertex $ Vertex3 x1 y1 0
                           vertex $ Vertex3 x2 y1 0
                           vertex $ Vertex3 x2 y2 0
                           vertex $ Vertex3 x1 y2 0
drawShape (S.Polygon pts (S.C r g b))
    = preservingMatrix $ do
        color $ Color3 r g b
        renderPrimitive Polygon $ mapM_ (\(x,y)->vertex $ Vertex3 x y 0) pts
         
drawShape (S.NoShape) = return ()    
red = S.C 1 0 0
                            
display anim curTime frameStats = do
 clear [ColorBuffer]
 loadIdentity
 t <- ((/1000) . realToFrac) `fmap` readTV curTime
 mayanim <- readTV anim
 (avg, n, mx, lst) <- readTV frameStats
 case mayanim of
   Just a -> mapM_ drawShape $ a t
   _ -> return ()
 if lst>0.0
    then do let tdif = t-lst
            writeTV frameStats ((avg*n+tdif)/(n+1), n+1, max mx tdif, t)
    else writeTV frameStats (0, 0, 0, t)
 if t>5 
    then leaveMainLoop
    else return ()
 --drawGreenCtrSquare 2 0.24
 --drawGreenCtrSquare (0.1/(t-5)) 0
 swapBuffers

idle startTime curTime = do
  newetime <- get elapsedTime 
  stime <- readTV startTime

  tlast <- readTV curTime

  if ((newetime-stime)-tlast>2)
     then do postRedisplay Nothing
             writeTV curTime (newetime-stime)
     else return ()
  --postRedisplay Nothing
reshape s@(Size w h) = do 
  viewport $= (Position 0 0, s)

cube w = do 
  renderPrimitive Quads $ do
    vertex $ Vertex3 w w w
    vertex $ Vertex3 w w (-w)
    vertex $ Vertex3 w (-w) (-w)
    vertex $ Vertex3 w (-w) w
    vertex $ Vertex3 w w w
    vertex $ Vertex3 w w (-w)
    vertex $ Vertex3 (-w) w (-w)
    vertex $ Vertex3 (-w) w w
    vertex $ Vertex3 w w w
    vertex $ Vertex3 w (-w) w
    vertex $ Vertex3 (-w) (-w) w
    vertex $ Vertex3 (-w) w w
    vertex $ Vertex3 (-w) w w
    vertex $ Vertex3 (-w) w (-w)
    vertex $ Vertex3 (-w) (-w) (-w)
    vertex $ Vertex3 (-w) (-w) w
    vertex $ Vertex3 w (-w) w
    vertex $ Vertex3 w (-w) (-w)
    vertex $ Vertex3 (-w) (-w) (-w)
    vertex $ Vertex3 (-w) (-w) w
    vertex $ Vertex3 w w (-w)
    vertex $ Vertex3 w (-w) (-w)
    vertex $ Vertex3 (-w) (-w) (-w)
    vertex $ Vertex3 (-w) w (-w)

points :: Int -> [(GLfloat,GLfloat,GLfloat)]
points n' = let n = fromIntegral n' in map (\k -> let t = 2*pi*k/n in (sin(t),cos(t),0.0))  [1..n]
