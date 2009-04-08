{-# OPTIONS -fglasgow-exts #-}

module Apparatus where

import Waves4
import Data.Maybe
import Data.List
import System.IO
import System.Time
import System.IO.Unsafe
import Control.Concurrent.STM
import Control.Monad.Identity
import Control.Monad.Trans
import NewResult
import Data.Typeable
import ArrayWave
--import Data.IORef

type RegWave = UVecWave Double

--type m :*: s = StateT s m

data Apparatus m o = Apparatus {
      run :: [o] -> m [(String, AnyResult)],
      newTrial :: [o] -> m (),
      prepare :: [o] -> m (),
      wait :: WaitCmd->m (),
      initialise :: m (),
      finalise ::  m (),
      finaliseTrial :: m ()
}
(>->) :: Monad m => (a->m c) -> (a->m b) -> a -> m b
do1 >-> do2 = \a -> do do1 a
                       do2 a

(>>->>) :: Monad m => (a->b->m c) -> (a->b->m d) -> a ->b-> m d
do1 >>->> do2 = \a b -> do do1 a b
                           do2 a b


act1 x = putStrLn $ "1: "++(show x)
act2 x = putStrLn $ "2: "++(show x)

beforeRun :: ApparatusM m s => ([o] -> m ()) -> Apparatus m o -> Apparatus m o 
beforeRun brun app = app {run = brun >-> run app }

afterRun :: ApparatusM m s => ([(String, AnyResult)] -> m [(String, AnyResult)]) -> Apparatus m o -> Apparatus m o 
afterRun arun app = app {run = run app >=> arun }

beforePrepare :: ApparatusM m s => ([o] -> m ()) -> Apparatus m o -> Apparatus m o
beforePrepare bprep app = app {prepare = bprep >-> prepare app }

debugWait :: (ApparatusM m s, MonadIO m) => Apparatus m o -> Apparatus m o
debugWait app = app { wait = (\wcmd-> liftIO $ print wcmd) >-> wait app }

afterPrepare :: ApparatusM m s => ([o] -> m ()) -> Apparatus m o -> Apparatus m o
afterPrepare aprep app = app {prepare = prepare app >-> aprep }

groundStateWaitFor :: ApparatusM m s => Double -> Apparatus m o -> Apparatus m o
groundStateWaitFor secs app = app {wait = \wcmd -> case wcmd of  
							WaitExactly tm -> wait app wcmd
							Relax -> wait app $ WaitExactly secs}

triggerWith :: ApparatusM m s => ([o] -> m ()) -> Apparatus m o -> Apparatus m o
triggerWith = beforeRun

prepareMap :: ApparatusM m s => (o -> m ()) -> Apparatus m o -> Apparatus m o
prepareMap prepo app = app {prepare = mapM prepo >-> prepare app }

constantResult :: (ApparatusM m s, Result r) => String -> r -> Apparatus m o -> Apparatus m o
constantResult nm res = afterRun $ \ress-> return $ (nm,AnyRes res):ress

sessionNum :: TVar Integer
sessionNum = unsafePerformIO $ mkCounter 0
trialNum :: TVar Integer
trialNum = unsafePerformIO $ mkCounter 0
lastTriggerTime = unsafePerformIO $ mkCounter Nothing

mkCounter n = newTVarIO n
incCounter ctr = atomically $ do v <- readTVar ctr 
                                 writeTVar ctr (v+1)
                                 return v
setCounter ctr n = atomically $ writeTVar ctr n
readCounter ctr = atomically $ readTVar ctr 

data WaitCmd = WaitExactly Double | Relax deriving Show

data Trial o = LeastWaitBefore Double
               | RelaxToGroundState
               | LeastDuration Double
               | Analyse [String] ([AnyResult]->Maybe AnyResult) String
               | Report String
	       | TrialName String
	       | TrialParameter String AnyResult
               | DoOption o
               | InjectResult String AnyResult
               deriving (Show, Eq)

data TrialDetails o = TD [Trial o] ClockTime Integer Integer

runOnApparatus :: (ApparatusM m s, Eq o) => Apparatus m o 
               -> (TrialDetails o -> [(String, AnyResult)] -> IO kt) 
               -> [Trial o] 
               -> Maybe s
               -> IO (kt,s)
runOnApparatus app rkont tss minist
    = do st1 <- case minist of
                     Just st -> return st
                     Nothing -> justSt . runInIO emptyState $ initialise app 
         lstTrig <- readCounter lastTriggerTime
	 tnow <- getClockTime
         prints "time now" tnow
         st1' <- justSt . runInIO st1 $ newTrial app [o | DoOption o <- tss ]
         st2 <- justSt . runInIO st1' $ prepare app [o | DoOption o <- tss ]
         st3 <- justSt . runInIO st2 $ wait app (delayCmd $ secDiff lstTrig tnow)
         trgTm <- getClockTime
         setCounter lastTriggerTime $ Just trgTm
         incCounter trialNum
         prints "trigger at" trgTm
         (ress, st4) <- runInIO st3 $ run app [o | DoOption o <- tss ]
         results <- return $! incrementAnal ress [(reqms, afun, rname) | (Analyse reqms afun rname) <- tss]  
   	 s <- readCounter sessionNum
	 t <- readCounter trialNum
         prints "trial number" t
         prints "total results" $ length results
	 repRess <- return $ filter ((`elem` [nm' | Report nm' <- tss]) . fst) results
                             --[(nm, rs) | (nm, rs) <- results, nm `elem` [nm' | Report nm' <- tss]] 
         prints "reporting results" $ length repRess
	 ret <- rkont (TD tss trgTm s t) repRess
	 st6 <- justSt . runInIO st4 $ finaliseTrial app
         return (ret, st6)

    where delayCmd elapsed = case RelaxToGroundState `elem` tss of
                               True -> Relax
                               False -> WaitExactly $ (foldr (max) 0.01 [t | LeastWaitBefore t <- tss ]) - elapsed
          secDiff (Just lstTrig) tnow = (diffInMicroS tnow lstTrig) / 1000000
          secDiff Nothing tnow = 0
          justSt = liftM snd 


diffInMicroS (TOD t1s t1ps) (TOD t2s t2ps) = fromInteger $ (t1s-t2s)*1000*1000 + ((t1ps-t2ps) `div` (1000*1000)) 

{-incrementAnal ::  [(String, r)] --existing results
              -> [([String], [r]->r, String)] --analyses to perform
              -> [(String, r)] -- new results
incrementAnal ress []    = ress
incrementAnal ress anals = case null todo of
                             True -> ress -- done, can't do rest
                             False -> incrementAnal (ress++(map doAnal todo)) postpone
    where (todo, postpone)      = partition canPerform anals  
          canPerform (nms, _, _) =  and $ map (`elem` map fst ress) nms
          doAnal (nreqs, lam, nres) = (nres, lam $ map (\nm->fromJust (lookup nm ress)) nreqs)
-}

analyse1 :: (Result a, Result b) => String->(a->b)->String->Trial o
analyse1 src f targ = Analyse [src] (\[AnyRes inres]->case cast inres `asTypeOf` undefA of
                                                        Just theA -> Just . AnyRes $ f theA 
                                                        Nothing -> Nothing) targ
                      where undefB = f undefA
                            undefA = undefined

analyse1m :: (Result a, Result b) => String->(a->Maybe b)->String->Trial o
analyse1m src f targ = Analyse [src] (\[AnyRes inres]->case cast inres `asTypeOf` undefA of
                                                        Just theA -> case f theA of
                                                                       Just theB -> Just $ AnyRes theB
                                                                       Nothing -> Nothing
                                                        Nothing -> Nothing) targ
                      where undefB = f undefA
                            undefA = undefined

class Monad m => ApparatusM m s | m -> s, s -> m where
	runInIO :: s -> m a -> IO (a,s)
        emptyState :: s


--class WaveToResult r where
--	waveToResult :: RegWave -> r

--instance ApparatusM Identity where
--	runInIO _ = return . runIdentity

comma x y = (y,x)

instance ApparatusM IO () where
	runInIO _ = liftM (comma ())
        emptyState = ()


instance Show (a->b) where
    show _ = "<function>"

instance Eq (a->b) where
    f == g = False

--reporters

discard :: TrialDetails o -> [(String, AnyResult)] -> IO ()
discard _ _ = return ()

prints s1 s2 = putStrLn (s1++": "++show s2)

nameOr tss defNm = [nm | TrialName nm <- tss] `headOr` defNm
    where   headOr [] y = y
            headOr (x:_) y = x

