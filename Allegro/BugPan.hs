module Allegro.BugPan where

import Data.List
import Allegro.Shape
import Allegro.Expr2
import System.Process
import System.Cmd
import System.Posix.Signals
import System.Posix.Types
import System.Posix.Process
import Control.Concurrent

initBugPan :: IO ProcessID
initBugPan 
   = do --system "killall -9 dyndriver 2>/dev/null"
   	--system "Allegro/dyndriver /home/tomn/braincurry/dynanim.so.1.0.1 &"
        --threadDelay $ 1000*1000
	system "ps -C gldriver -o pid h >.dyndriver_pid"
        --threadDelay $ 250*1000
        putStr "init bugpan, pid="
	pid <- read `fmap` readFile ".dyndriver_pid"
        print pid
        return pid
        
launchBugPan 
    = do system "killall -9 gldriver 2>/dev/null"
         system "Allegro/gldriver /home/tomn/braincurry/dyngl.so.1.0.1 &"
         threadDelay $ 1000*1000
	
	
{-p <-forkProcess $ executeFile "dyndriver" False [] Nothing
	threadDelay $ 6000*1000
	print p
	system "ps aux | grep dyndriver"
	return p -}


prepareAnim :: [Declare] -> Double -> ProcessID -> IO ()
prepareAnim e tmax pid
   = do compileExpr e tmax
	signalProcess 10 pid
        threadDelay $ 1000*1000
	--system "killall -s 10 dyndriver" >> return ()

playAnim :: ProcessID ->IO ()
playAnim  pid = do putStrLn $ "signalling 12 (go) to pid "++show pid
                   signalProcess 12 pid

--system "killall -s 12 dyndriver"  >> return ()
killBugPan :: ProcessID ->IO ()
killBugPan = signalProcess 9

compileExpr :: [Declare] -> Double -> IO ()
compileExpr e tmax 
    = do let cstr = padExpr (compile e) tmax
         writeFile "dyngl.c" cstr
         system "gcc -fPIC -g -c dyngl.c"
         system "gcc -shared -Wl,-soname,dyngl.so.1 -o dyngl.so.1.0.1 dyngl.o -lc"
         return ()

{-exprToCcode :: Expr -> Double -> String
exprToCcode e tmax = concat ["#include \"Allegro/dynanim.h\"\n\n", 
                             "double tmax_s() {return ", show tmax, ";}\n",
                             "void the_animation(SDL_Surface *scr, double tnow) {\n",
                             toCe e True, 
                             "\n}\n"]
-}
padExpr e tm = concat ["#include <GL/glfw.h>\n", "#include <stdlib.h>\n",
                       "#include <stdio.h>\n","#define max(a,b) (((a) > (b)) ? (a) : (b))\n",
                       "#define min(a,b) (((a) < (b)) ? (a) : (b))\n",
                       "double tmax_s() {return "++(show tm)++";}\n",
                       e]
