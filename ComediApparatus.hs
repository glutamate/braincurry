{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -fglasgow-exts -fno-monomorphism-restriction #-}

module ComediApparatus where

import Apparatus hiding (prints)

import Comedi hiding (actualizeChannel )
import GHC.Conc (threadDelay, forkIO)
import Control.Monad.State.Lazy
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array
import Waves4
import NewResult
import Data.Array.Vector
import ArrayWave

data AcqPromise = WvProm { promNm :: String, promChanN :: Integer, promNpnts :: Int, promDeltat :: Double, 
                            promDecim :: Int } deriving Show
data ComediState = CS [AcqPromise] Bool-- change to list of device ptr, with dev names

newtype ComediApparatusM a = ComediApparatusM { unCAM :: StateT ComediState IO a }
     deriving (Monad, MonadIO, MonadState ComediState)

instance ApparatusM ComediApparatusM ComediState where
    runInIO s ma = do (a,news) <- runStateT (unCAM ma) s
                      return (a,news)
    emptyState = CS [] False

{-setDevPtr :: Ptr () -> ComediApparatusM ()
setDevPtr p = do (CS _ r outp) <- get 
                 put (CS (Just p) r outp)

getDevPtr :: ComediApparatusM (Ptr ())
getDevPtr = do (CS (Just p) _ _) <- get 
               return p
-}
addWavePromise :: AcqPromise -> ComediApparatusM ()
addWavePromise pr = do (CS r outp) <- get 
                       put (CS (pr:r) outp)

clearPromises :: ComediApparatusM ()
clearPromises = do (CS _ outp) <- get 
                   put (CS [] outp)

getOutput :: ComediApparatusM Bool
getOutput = do (CS prs hasOut) <- get 
               return hasOut

setOutput :: Bool -> ComediApparatusM ()
setOutput outv = do (CS prs _) <- get 
                    put (CS prs outv)


getAllWaves :: ComediApparatusM [(String,AnyResult)]
getAllWaves 
    = do (CS r _) <- get
         liftIO $ mapM prom2res r

prom2res (WvProm nm nchan npnts delt dn) 
    = do ptr <- get_wave_ptr (fromInteger nchan)
         lst <- peekArray npnts ptr
	 let newUVec = toU $ map toDouble lst
         --putStr $ "decimate by "++show dn
         return (nm, AnyRes {-. decimate dn -}$ UVecWave (newUVec) 
                                                        delt 
                                                        0.0
                                                        (lengthU newUVec))
         where toDouble :: Real a => a -> Double
               toDouble = realToFrac
		

actualizeChannel :: Chan -> ComediApparatusM Chan
actualizeChannel (AnalogChannel subDevType rngSpan rtHz chanNum)
    = do --pt <- getDevPtr
         sdt <- liftIO $ findSubdeviceByType  subDevType
         r <- liftIO $ findRange  sdt chanNum rngSpan
         --prints "new chan " $ Chan devPtr sdt chanNum rng rtHz devnm
         case subDevType of
           AnalogInput -> return $ AIChan sdt chanNum r rtHz 
           AnalogOutput -> return $ AOChan sdt chanNum r rtHz 

actualizeChannel (DigitalChannel subDevType chan) 
    = do pt <- liftIO get_comedi_ptr 
	 sdt <- liftIO $  findSubdeviceByType DigitalIO
	 let dir = case subDevType of
		     DigitalOutput -> 1 
		     DigitalInput -> 0
         
	 liftIO $comedi_dio_config pt (fI sdt) (fI chan) (fI dir)
		--putStrLn "done actualizig dig"
         return $ DIOChan sdt chan dir
actualizeChannel c = return c

comediApparatus rtHz
    = Apparatus {
        run = (\opts -> do -- p<- getDevPtr
                           hasOut <- getOutput
                           when hasOut (liftIO ((forkIO $ start_cont_output ) >> return ()))
                           liftIO $ start_cont_acq 
                           getAllWaves
                  ),
        newTrial = const $ do -- p<- getDevPtr
                              sdt <- liftIO $ findSubdeviceByType  AnalogInput
                              liftIO  $ putStrLn ("using subdev "++show sdt)
                              liftIO $ new_trial (fromIntegral $ sdt) 
                                                 (fromIntegral $ rtHz)
                              clearPromises
                              setOutput False
                              return (),

        prepare = (\opts -> do -- p<- getDevPtr
                               liftIO $ prepare_cont_acq 
                               return ()
                  ),
        wait = (\wcmd-> case wcmd of 
                          WaitExactly secs -> liftIO $ threadDelay . round $ max (secs*1000000) 0
                          Relax -> liftIO $ threadDelay $ 1*1000000),
        initialise = return (), -- do ptr <- liftIO $ open "/dev/comedi0"
                        -- setDevPtr ptr,
        finalise = return (),
        finaliseTrial = liftIO $free_trial_results
      }


--putOutputWave

readWave :: Chan -> String -> Double -> ComediApparatusM ()
readWave ch' nm reclen = do ch <- actualizeChannel ch'
                            --liftIO $ prints "specCh" ch'
                            --liftIO $ prints "actCh" ch
                            globFreq <- liftIO $ getGlobalFreq
                            let delt = (1.0/(realToFrac globFreq)) -- $ acqRateHz ch'))
                            let npts = round $ reclen/delt
                            let dn = round (globFreq / realToFrac (acqRateHz ch')) -1
                            chNum <- liftIO $ setupReadWave ch npts
                            let prom = WvProm nm (toInteger chNum) npts delt dn
                            addWavePromise prom
                            --liftIO $ prints "promise" prom
                            return ()

writeWave :: Chan -> UVecWave Double -> ComediApparatusM ()
writeWave ch' w = do ch <- actualizeChannel ch'
                     hasOut <- getOutput
                     when (not hasOut) $ do 
                       -- p <- getDevPtr
                       globFreq <- liftIO $ getGlobalFreq
                       liftIO $ new_trial_out (fromIntegral $ subDev ch) (realToFrac globFreq)
                       setOutput True 
                     let dat = map realToFrac . fromU . uVpnts $ w
                     liftIO $ setupWriteWave ch dat 


digitalPulse :: Chan -> ComediApparatusM ()
digitalPulse ch' = do ch <- actualizeChannel ch'
                      liftIO $ beep ch
digitalPulses :: [Chan] -> ComediApparatusM ()
digitalPulses chs' = do chs <- mapM actualizeChannel chs'
                        liftIO $ multiDigPulse chs


