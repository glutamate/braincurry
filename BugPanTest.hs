module Main where

import Allegro.BugPan
import Allegro.Expr2
import Control.Concurrent


main = do pid <- initBugPan
	  print pid 
	  threadDelay $ 1000*1000
	  prepareAnim (loom 0.5 0) 6.0 pid
	  print "done preparing"
	  threadDelay $ 1000*1000
	  playAnim pid
	  threadDelay $ 6000*1000
	  killBugPan pid

--compileExpr loom2 6.0 >> init_dynanim_driver >> open_anim >> play_anim >>= print >> close_anim