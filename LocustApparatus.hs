{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Waves4
import Apparatus
import SimApparatus
import SimNeuron
import System.IO
import ComediApparatus
import Comedi hiding (actualizeChannel, prints)
import Event
import SaveWaves
import LocustTypes
import ApparatusPersistence
import NewResult
import Allegro.Expr2
import BugPanApparatus
import Control.Monad.Trans (liftIO)
import System.Environment
import ArrayWave
import System.Random
import Control.Monad

newSessn = newSessionFromSQL
continueSessn = continueLastSession

main = do
  getArgs >>= dispatch 


dispatch ("-n":sessName:rest) = newSessionFromSQL sessName >> dispatch rest 
dispatch ("-c":rest) = continueLastSession >> dispatch rest 
dispatch ("-a":lov:rest) = print "foobar" >>runOnApparatus dcmdRig ( logToScreen >>->> showLocally >>->> discard) (tryTrial (Const $ read lov)) Nothing >> ok
dispatch ("-s":freq:amp:dur:rest) = print "foo" >> runOnApparatus dcmdRig ( logToScreen >>->> showLocally >>->> discard) tr Nothing >> ok
    where tr = (DoOption $ PlaySquareSound (read freq) (read amp) (read dur)):approachTrial

dispatch ("habit2004":rest) = expt habit2004Trials 10 >> ok
dispatch ("habitFast":rest) = expt (withName "habitFast" habitTrials) 20 >> ok
dispatch ("sndDishabit":rest) = do trs <- sndRndTrs
                                   expt (trs) 1 
                                   ok
dispatch ("displaced":rest) = do trs <- displacedTrials
                                 expt trs 1 
                                 ok

dispatch ("twoOuts":rest) = twoOuts >> ok
dispatch []   = ok

ok =  return ()
                            

--- DCMD rig

--dcmdRig :: Apparatus ComediApparatusM LocustOptions
dcmdRig = withBugPan locustAnims $
          groundStateWaitFor (8*60) $ debugWait $
          prepareMap dcmdRigPrepare $
	  triggerWith dcmdTrigger $
          comediApparatus 20000

dcmdTrigger opts -- = digitalPulses (digChan 0:(concatMap selApproachVideo opts))
    = liftIO $ internal_trigger
selApproachVideo (Approach lov) | lov `near` 5.0 = [digChan 1, digChan 2]
                                | lov `near` 10.0 = [digChan 2]
                                | lov `near` 20.0 = [digChan 1]
                                | otherwise = []
selApproachVideo _ = []


near :: (Fractional a, Ord a) => a -> a -> Bool
near a b | abs(a-b)<0.001 = True
         | otherwise = False

dcmdRigPrepare RecordEC = {-readWave monitorOutChannel "monitorVoltage" 6.0 >> -} 
                          readWave silverElectrode "ecVoltage" 6.0 
dcmdRigPrepare (PlaySound w) = writeWave speakerOutChannel w
dcmdRigPrepare (PlaySquareSound freq amp dur) 
    = do --liftIO $ plotRegW w
         writeWave speakerOutChannel sqrw
             where sqrw = (*amp) `wmap` (sqrUWave (1/freq) dur (1/20000))
                   pwmw = (*amp) `wmap` (pwmUWave 0.001 (1/freq) dur (1/20000))
dcmdRigPrepare _ = return ()


--dcmdProcess wvs = return $ map (\(n,w) -> (n,ResWave w)) wvs

silverElectrode = AnalogChannel AnalogInput 
				(-10,10) 20000 0
monitorOutChannel = AnalogChannel AnalogInput 
				  (-10,10) 20000 1
speakerOutChannel = AnalogChannel AnalogOutput 
				  (-10,10) 20000 0
digChan n = DigitalChannel DigitalOutput n

locustAnims :: [LocustOptions] -> Maybe ([Declare], Double)
locustAnims os = safeHead [ (e,tmax) | (PlayAnimation e tmax)<- os ]
--- Model

dcmdModel :: Apparatus NeuronSimM LocustOptions
dcmdModel = addWhiteNoise 0.001 20e-12 $
            constantResult "collisionTime" (5.0::Double) $ 
            prepareMap dcmdModelPrepare $ 
            intFireApparatus 0.001 grcNeuron

dcmdModelPrepare RecordEC = tellDuration 6
dcmdModelPrepare (Approach lov) = addPoissonSynapse (alphaGmax 2e-12 800) 0 (shift 5 $ predictf 50 0.01 0.1 (lov/1000)) noPlast' 6
--dcmdModelPrepare (Approach lov) = addCurrent $ iInj (shift 4 $ step (lov*3e-12) 1)
dcmdModelPrepare _ = return ()

stepProb lov = shift 4 $ step (lov*5) 1 -- 200 hz at lov=40

theta lov t = -2*(atan (lov/t))*(360/(2*pi))
dtheta lov t = lov/(t^2 + lov^2)*(360/(2*pi))
predictf c delay alpha lov t | t<0 = c*(abs(dtheta lov (t-delay)))*exp(-1*alpha*theta lov (t-delay))
                             | t>=0 = 0
gauss m sd x = (1/(sd*sqrt(2*pi)))*exp((-(x-m)^2)/(2*sd^2))

--plotFun (predictf 2.62e-4 0.042 5.73 0.0020) (-100) 0

--trgDCMD (dvPtr,_) = let digC = DigitalChannel "/dev/comedi0" DigitalOutput 0 in
tryTrial lov =((DoOption $ PlayAnimation (loom (lov/1000)) 6.0):approachTrial) 
tryModel = runOnApparatus dcmdModel (showLocally >>->> discard) (tryTrial 40) Nothing
tryApp = runOnApparatus dcmdRig ( logToScreen >>->> showLocally >>->> discard) (tryTrial 40) Nothing

runManyOnApparatus app persist [] s = return ()
runManyOnApparatus app persist (tr:trs) s = do  (_,s') <- runOnApparatus app persist tr s
                                                rest <- runManyOnApparatus app persist trs (Just s')
                                                return ()

beepDigs = return () --multiDigPulse [digChan 0, digChan 1, digChan 2, digChan 3]

multiApproach :: Integer -> Double -> [Trial o] -> [[Trial o]]
multiApproach n secSep tr =     let     pulsPar n = TrialParameter "approachNum" $ AnyRes n
                                        pulsFreq = TrialParameter "pulseSepSecs" $ AnyRes secSep
                                        first = tr++[pulsFreq, RelaxToGroundState, pulsPar (1 :: Integer)]
                                        rest n = tr++[pulsFreq, LeastWaitBefore secSep, pulsPar n] in
                                first:(map rest [2..n])

--habitExpt :: [[TrialSpec]]
habitTrials = do  spd <- [0.01,0.02]
                  sepSec <- [15,30]
                  multiApproach 10 sepSec $ approachTrial++[DoOption $ PlayAnimation (loom spd) 6.0]
--repDeep :: Integer -> [[a]] -> [[

habit2004Trials = multiApproach 30 60 $ approachTrial++[DoOption $ PlayAnimation (loom 0.02) 6.0, 
                                                        TrialParameter "approachLoV" $ AnyRes (0.02::Double),TrialName "habit2004"]
                  

(*>) :: Result a => String -> a -> Trial o
str *> res = TrialParameter str $ AnyRes res

withName nm trs = map (TrialName nm:) trs

--sndAppTrial :: 
sndAppTrial n= [DoOption $ PlayAnimation (loom 0.01) 6.0, 
                TrialParameter "approachNum" $ AnyRes (n::Integer),
                TrialParameter "approachLoV" $ AnyRes (10::Integer),
                DoOption RecordEC,
                Report "ecVoltage"]

playSnd amp = [LeastWaitBefore 15, DoOption $ PlaySquareSound 200 amp 1]

sndDisPulse True amp =  map ((sndPar amp)++) [RelaxToGroundState : sndAppTrial 1,
                                                  LeastWaitBefore 30 : sndAppTrial 2,
                                                  LeastWaitBefore 30 : sndAppTrial 3,
                                                  playSnd amp ++ [ DoOption RecordEC, Report "ecVoltage"],
                                                  LeastWaitBefore 15 : sndAppTrial 4]
sndDisPulse False _ = map (TrialName "nosound":) [RelaxToGroundState : sndAppTrial 1,
                                                  LeastWaitBefore 30 : sndAppTrial 2,
                                                  LeastWaitBefore 30 : sndAppTrial 3,
                                                  LeastWaitBefore 30 : sndAppTrial 4]


sndPar a = [TrialParameter "soundAmp" $ AnyRes a, 
            TrialName "sound", 
            TrialParameter "soundAmpmV" $ AnyRes ((round $ 1000*a)::Int)]

soundDishabitTrials = concatMap (sndDisPulse True) amps

sndRndBlock :: IO [[Trial LocustOptions]]
sndRndBlock = do amp <- oneOf amps
                 return $ sndDisPulse True amp


cycleNM :: Int -> IO [[a]] -> IO [[a]]
cycleNM n mxss = do xss <- sequence $ replicate n mxss
                    return $ concat xss


sndRndTrs :: IO [[Trial LocustOptions]]
sndRndTrs =  cycleNM 200 sndRndBlock 
             {-do block <- sndRndBlock 
                more <- sndRndTrs
                return $ block++more -}

displTr d lov = [DoOption RecordEC,
	         Report "ecVoltage",
                 LeastWaitBefore 60,
                 TrialName "displacedLoom",
                 TrialParameter "approachLoV" $ AnyRes (lov),
                 TrialParameter "loomDisplacement" $ AnyRes (d),               
                 DoOption $ PlayAnimation (loomDisplaced (Const d) (Const lov)) 6.0]

aDisplTr = return (displTr) `ap` randomRIO (-1,1) `ap` oneOf [0.01,0.02, 0.04]
             
displacedTrials = sequence $ replicate 500 aDisplTr

oneOf lst = do ind <- randomRIO (0,length lst - 1)
               return $ lst!!ind
  
expt trs n = runManyOnApparatus dcmdRig (storeInSQL >>->> logToFile >>->> discard) (concat $ replicate n trs) Nothing

addToEach :: a -> [[a]] -> [[a]]
addToEach x lsts = map (x:) lsts

twoOuts = runManyOnApparatus dcmdRig ( logToScreen >>->> showLocally >>->> discard) (cycle [appTr, outTr]) Nothing
    where outTr = [DoOption RecordEC,Report "ecVoltage", LeastWaitBefore 30,DoOption $ PlaySquareSound 200 1 1]
          appTr = [DoOption RecordEC,Report "ecVoltage", LeastWaitBefore 30,DoOption $ PlayAnimation (loom 0.04) 6.0]

latinSqrs [] = []
latinSqrs xs | even $ length xs = head xs:(latinSqrs $ tail xs)
             | otherwise = last xs : (latinSqrs $ init xs)

amps = {-latinSqrs $-} take 10 $ iterate (*1.50) 0.05