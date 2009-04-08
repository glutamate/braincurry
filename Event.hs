{-# LANGUAGE NoMonomorphismRestriction #-}

module Event where

import Waves4
import ArrayWave
--import System.IO.Unsafe
import Control.Arrow ((>>>))

infixr 3 .&&.
(.&&.) :: (a-> Bool) -> (a-> Bool) -> (a-> Bool)
f1 .&&. f2 = \x -> (f1 x) && (f2 x)

infixr 2 .||.
(.||.) :: (a-> Bool) -> (a-> Bool) -> (a-> Bool)
f1 .||. f2 = \x -> (f1 x) || (f2 x)

data Event a = Event {etime ::Double, einfo :: a} | EventMissingInfo {etime ::Double}

type EventDetector wt vt a = (wt vt, [Event a]) -> (wt vt, [Event a])

wpnts w = [0..(npnts w)-1]
tpnts w = map ((*(dt w)) . realToFrac) $ wpnts w
pcntT pcnt wv = pcnt*(maxt wv-mint wv) + mint wv

--pickWhen :: (DiscreteWave wt, RealFrac tt) => (wt tt vt -> vt) -> (vt -> Bool) -> EventDetector wt tt vt a

--pickWhenAmplitude :: (DiscreteWave wt vt) => (vt -> Bool) -> EventDetector wt vt a
pickWhenAmplitude vpred (wv, existEvts) = (wv, newEvts++existEvts)
	where yesPnts = filter (vpred . (wv `atpnt`)) (wpnts wv)
	      newEvts = map (EventMissingInfo . p2t wv) yesPnts

--pickWhenAmpDivSD :: (Floating vt, DiscreteWave wt vt) => (vt -> Bool) -> EventDetector wt vt a
pickWhenAmpDivSD vpred (wv, existEvts) = (wv, newEvts++existEvts)
	where 	sd = sdev wv 
		yesPnts = filter (vpred . (/sd) . (wv `atpnt`)) (wpnts wv)
	      	newEvts = map (EventMissingInfo . p2t wv) yesPnts
		

--restrict w (evtm+t1) (evtm+t2)

--pickDerivSpikes :: Double -> (Double,Double) -> EventDetector RegularWave Double a
pickDerivSpikes n (pc1, pc2) (wv, existEvts) = (wv, newEvts++existEvts)
	where 	deriv = derivative wv
		sd = sdev $ restrict deriv (pcntT pc1 deriv) (pcntT pc2 deriv) 
                dt' = dt wv
                zipvls w = zip (map ((*dt') . realToFrac) $ wpnts w) (pntsAsList w)
		yesTimes= map (fst) $ filter (((>n) .||.(< (-n))) . (/sd) . snd) (zipvls deriv)
	      	newEvts = map (EventMissingInfo) yesTimes

pickPosDerivSpikes n (pc1, pc2) (wv, existEvts) = (wv, newEvts++existEvts)
	where 	deriv = derivative wv
		sd = sdev $ restrict deriv (pcntT pc1 deriv) (pcntT pc2 deriv) 
                dt' = dt wv
                zipvls w = zip (map ((*dt') . realToFrac) $ wpnts w) (pntsAsList w)
		yesTimes= map (fst) $ filter ((>n) . (/sd) . snd) (zipvls deriv)
	      	newEvts = map (EventMissingInfo) yesTimes

withMeanSD meanSdEDlambda (wv, existEvts) = meanSdEDlambda (mean wv, sdev wv) (wv, existEvts)

filterAllEvents :: (Event a -> Bool) -> EventDetector wt vt a
filterAllEvents p (w, existEvts) = (w, filter p existEvts)

filtEvts :: (a -> Bool) -> EventDetector wt vt a
filtEvts p (w, existEvts) = (w, filter (\e-> case e of
						Event _ a -> p a
						e -> True) existEvts)


completeEvents = filterAllEvents hasInfo

hasInfo (Event tt a) = True
hasInfo _ = False

{-measure :: (wt tt vt -> tt-> Maybe a) -> EventDetector wt tt vt a
measure f (w, existEvts) = (w,(>3) .||. (< -3) map (\e -> case f w $ etime e of
						Just i -> Event (etime e) i
						Nothing -> e) existEvts)
-}
refineTime :: (wt vt -> Event a-> Maybe Double) -> EventDetector wt vt a
refineTime f (w, existEvts) = (w, map (\e -> case f w e of
						Just t -> e {etime=t}
						Nothing -> e) existEvts)

locally' :: (FiniteWave wt vt) 
	=> (wt vt, Event a) -> (Double,Double) -> wt vt
locally' (w, e) (t1, t2) = restrict w (evtm+t1) (evtm+t2)
	where evtm = etime e

locally :: (FiniteWave wt vt) 
	=> (Double,Double) -> EventDetector wt vt a -> EventDetector wt vt a
locally (t1, t2) ed (w, existEvts) = (w, evts)
	where 	evts = concatMap (snd . ed . (withWave)) existEvts
		withWave e = (locally' (w,e) (t1, t2),[e]) 
	--pairWSnds :: (a,[b])-> [(a,b)]
	--pairWSnds (x, ys) = map (x,) ys

leastInterval :: (RealFloat vt, FiniteWave wt vt) => Double -> EventDetector wt vt a
leastInterval refrac (w, []) = (w, [])
leastInterval refrac (w, evts) = (w,(head evts):map snd goodPairs)
	where 	goodPairs = filter (\(e1,e2)->(etime e2 - etime e1)>refrac) $ zip evts (tail evts)

--spikeD = pickWhenAmpDivSD ((>3) .||. (< -3)) >>> locally (-2, 2) (waveMustHave ((>5) . maxval)) >>> completeEvents


waveMustHave :: (wt vt -> Bool) -> EventDetector wt vt a
waveMustHave wcond (w, evts) = if wcond w 
				then (w, evts)
				else (w, [])
notAtEnds:: (FiniteWave wt vt) => (Double,Double) -> EventDetector wt vt a
notAtEnds (t1, t2) (w, evts) = (w, filter ((>t1). etime) . filter ((<((maxt w)-t2)). etime) $ evts)

--relToEvt :: (t1, t2) -> (wt tt vt -> Bool)

--stepEvt :: (RealFrac tt, DiscreteWave wt, RealFloat vt) =>  tt-> (vt -> Bool) -> EventDetector wt tt vt a
stepEvt tm = pickDerivSpikes 6 (0.1,0.7) >>> notAtEnds (tm, tm) >>> onlySteps tm
			--	>>> locally (0,tm) (waveMustHave (valpred . mean))

eprints s1 s2 = putStrLn (s1++": "++show s2)

{-crossesUp vl (w,exEvts) = (w, hits++exEvts)
    where   diffs = zip3 (pnts w) (tail $ pnts w) (tpnts w)
            hitPts = filter (\(y1,y2,_) -> y1 <vl && y2>vl) diffs
            hits = map (\(_,_,t) -> EventMissingInfo t) hitPts-}

onlySteps tl (w, evts) = (w, filter isStep evts)
    where isStep e = let w1 = locally' (w, e) ((-tl),0)
                         w2 = locally' (w, e) (0,tl) in
		     abs(mean w1 - mean w2)>6*(sdev w1)

--spikeD :: Wave wt Double => Double -> EventDetector wt Double a
spikeD minamp = pickPosDerivSpikes 4 (0.1,0.2)
--                      >>> locally (-0.002,0.002) (waveMustHave (((>thpos) . maxval) .&&. ((<thneg) . minval)))
                      >>> locally (-0.001,0.001) (waveMustHave (\w->(maxval w - minval w)>minamp))
                      -- >>> leastInterval 0.002
             -- >>> locally (-0.001,0.001) (waveMustHave ((==0) . mean))
--tsSpk = do wv <- loadWave "dcmd_50khz.wv"
--           (w1, es) <- return $ spikeD (-0.4) 0.1 (wv, [])
--           print $ map (etime) es
