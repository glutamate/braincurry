{-# OPTIONS -fglasgow-exts #-}

module ParApparatus where

import Control.Concurrent
import Apparatus


{-
data Apparatus m o = Apparatus {
      run :: [o] -> m [(String, AnyResult)],
      newTrial :: [o] -> m (),
      prepare :: [o] -> m (),
      wait :: WaitCmd->m (),
      initialise :: m (),
      finalise ::  m (),
      finaliseTrial :: m ()
}

class Monad m => ApparatusM m s | m -> s where
	runInIO :: Maybe s -> m a -> IO (a,s)
-}

inParallel :: Apparatus m o -> Apparatus m o -> Apparatus m o
inParallel app1 app2 
    = Apparatus {
        initialise = do 

      }