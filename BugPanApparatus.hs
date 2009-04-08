{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
module BugPanApparatus where

import Apparatus hiding (prints)
import System.Posix.Types
import Control.Monad.State.Lazy
import Allegro.BugPan hiding (prepareAnim, playAnim)
import qualified Allegro.BugPan as BP
import Allegro.Expr2 
data BugPanState s = BPS ProcessID Bool s

instance ApparatusM m s => 
    ApparatusM (StateT (BugPanState s) m) (BugPanState s) where
        runInIO bps@(BPS _ _ s) bpma 
            = do ((a, BPS npid nact _), ns) <- runInIO s (runStateT (bpma) bps)  
                 -- (a,nns) <- runInIO (ma) ns 
                 return (a, BPS npid nact ns)
                                           
        emptyState = BPS 0 False emptyState

setPid :: (Monad m) => ProcessID -> StateT (BugPanState s) m ()
setPid pid = do (BPS _ act s) <- get
                put (BPS pid act s)

getPid :: (Monad m) => StateT (BugPanState s) m ProcessID
getPid = do (BPS pid _ _) <- get
            return pid

setBugPanActive :: (Monad m) => Bool -> StateT (BugPanState s) m ()
setBugPanActive act = do (BPS pid _ s) <- get
                         put (BPS pid act s)

getBugPanActive :: (Monad m) => StateT (BugPanState s) m Bool
getBugPanActive = do (BPS _ act _) <- get
                     return act

prepareAnim e tm = do pid <- getPid
                      liftIO $ BP.prepareAnim e tm pid
                      setBugPanActive True

playAnim :: (MonadIO m) => StateT (BugPanState s) m ()
playAnim = do pid <- getPid
              liftIO $ BP.playAnim pid

withBugPan :: (ApparatusM m s, MonadIO m) => ([o]->Maybe ([Declare], Double)) -> Apparatus m o -> Apparatus (StateT (BugPanState s) m) o
withBugPan getanim app = Apparatus {
                           initialise = do liftIO initBugPan >>= setPid
                                           lift $ initialise app,
                           finalise = lift $ finalise app,
                           finaliseTrial = lift $ finaliseTrial app,
                           run = \os -> do hasAnim <- getBugPanActive
                                           when hasAnim playAnim
                                           lift $ run app os,
                           newTrial = lift2 $ newTrial app,
                           prepare = \os -> do case getanim os of
                                                 Just (e, tm) -> prepareAnim e tm
                                                 Nothing -> setBugPanActive False
                                               lift $ prepare app os,
                           wait = lift2 $ wait app
                 }

lift2 :: (MonadTrans t, Monad m) => (a -> m b) -> (a -> t m b)
lift2 act = \a-> lift $ act a  