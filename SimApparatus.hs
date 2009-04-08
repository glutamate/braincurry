{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -fglasgow-exts -fno-monomorphism-restriction #-}

module SimApparatus where

import Apparatus
import Waves2
import SimNeuron
import SaveWaves
import Control.Monad.State.Lazy
import System.Random
import System.IO.Unsafe
import Control.Monad.Fix
import NewResult

intFireApparatus delt ifn = Apparatus {
      run = \_ -> do is <- getCurrents
                     let n = foldl (.+.) ifn is
                     tmax <- getDuration
                     let vms = intFire n delt tmax
                     return [("somaVm", AnyRes $ wvFromNumList delt vms)],

      newTrial = const zeroDuration, 
      prepare = const $ return (),
      wait = \cmd -> case cmd of 
                          Relax -> setVm $ vreset ifn
                          WaitExactly secs -> let waitVms = intFire ifn delt secs in
                                              setVm $ last waitVms,
      initialise = setVm $ vreset ifn,
      finalise =  return (),
      finaliseTrial = clearCurrents 
}

newtype NeuronSimM a = NeuronSimM { unNeuronSimM :: State NeuronSimST a }
    deriving (Monad, MonadFix, MonadState NeuronSimST)

data NeuronSimST = NST {nstCurrents :: [Current Double], ntsVm :: Double, nstDuration:: Double, nstRndGen :: Maybe StdGen}

instance ApparatusM NeuronSimM NeuronSimST where
        runInIO (NST cs v d Nothing) nsm = do   g <- getStdGen
                                                return $ runState (unNeuronSimM nsm) (NST cs v d (Just g))
        runInIO s nsm = -- do (a,news) <- runState (unNeuronSimM ma) s
                          return $ runState (unNeuronSimM nsm) s --(a,news)
        emptyState = NST [] (-0.06) 0 Nothing
    
setVm vm = do (NST cs _ d g) <- get 
              put (NST cs vm d g)

--get_ f = return . f =<< get  

getVm vm = do (NST _ v _ g) <- get 
              return v

rnd rng = do        s@(NST cs v d (Just g)) <- get 
                    let (x,g') = randomR rng g
                    put s { nstRndGen = Just g' }
                    return x

ran' rgenf = do     s@(NST cs v d (Just g)) <- get
                    let (g1,g2) = split g
                    let xs = rgenf g1
                    put s { nstRndGen = Just g2 }
                    return xs

rndRs rng = ran' (randomRs rng)
rnds = ran' randoms

grans m sd (x1:x2:xs) = let (g1,g2) = toGaussian (x1,x2) in
                        (sd*g1+m):(sd*g2+m):(grans m sd xs)

toGaussian (u1,u2) =  let   sqp = sqrt(-2*log(u1))
                            thp = 2*pi*u2 in
                      (sqp*cos(thp), sqp*sin(thp))


gnoise :: Double -> Double ->NeuronSimM [Double]
gnoise m sd = return . (grans m sd) =<< rndRs (0,1)


tstNSM :: (NeuronSimM a) -> (a->b) -> IO b
tstNSM ma f = do  (r,_) <- runInIO emptyState (ma >>= return . f) 
                  return $ r

tstStrCurr = tstNSM (do infnoise <- gnoise 0 1e-12
                        (Current cf0) <- return $ streamCurrent 0.1 infnoise
                        (v0,Current cf1) <- return $ cf0 0 0
                        (v1,Current cf2) <- return $ cf1 0.09 0
                        (v2,Current cf3) <- return $ cf2 0.11 0
                        return (v0, v1, v2)) id


                        
{-unsafeRnds sd = unsafePerformIO (ioRndGuassian sd) : (unsafeRnds sd) -- Works! 
ioRndGuassian sd = do   u1<- randomRIO (0,1)
                        u2<- randomRIO (0,1)
                        return . (*sd) . fst $ toGaussian (u1,u2)
--repeatM (gran1 sd)--return $ unsafeRnds 1e-12--infMList5--gnoise 0 sd return $ repeat 1e-12 --- -}

addWhiteNoise delt sd app = beforeRun (\_-> do  infnoise <- gnoise 0 sd
                                                addCurrent $ streamCurrent delt infnoise ) app

addPoissonSynapse gt erev pOfT plas tend
    = do rs <- rndRs (0,1)
         let spks = poissonSpikes pOfT 0.001 rs tend
         addCurrent $ synapse gt erev spks plas

addCurrent i = do   (NST cs v d g) <- get 
                    put (NST (i:cs) v d g)

clearCurrents = do (NST _ v d g) <- get
                   put (NST [] v d g)

getCurrents = do (NST is _ _ g) <- get 
                 return is

zeroDuration  = do (NST cs v _ g) <- get 
                   put (NST cs v 0 g)

getDuration  = do (NST _ _ d g) <- get 
                  return d

tellDuration tm = do (NST cs v d g) <- get 
                     put (NST cs v (max d tm) g)
