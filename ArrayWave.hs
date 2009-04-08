{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, PatternSignatures, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
{-# OPTIONS -fbang-patterns #-}

module ArrayWave where

import Data.Maybe
import Waves4
import Data.Array.Vector
import Data.Typeable
import Data.Ord

data UA vt => UVecWave vt = UVecWave {uVpnts :: UArr vt, 
                                      uVstept :: !Double, 
                                      uVoffset :: !Double,
				      uVnumpnts :: !Int} 
                         deriving (Read, Eq, Typeable)

instance Show a => Show (UVecWave a) where
    show uv = "<uvecwave>"

uWvNfirst n w = (take n $ fromU (uVpnts w))

mkUVecWave :: (Wave UVecWave a,UA a) => [a] -> Double  -> Double -> UVecWave a
mkUVecWave pts dt off = UVecWave (toU pts) (dt) off (length pts)

funToUVecWave dt maxt f = mkUVecWave pts dt 0
    where pts = map f $ enumFromThenTo 0 dt maxt

realmod :: (RealFrac a) => a->a->a
realmod x y = x-((realToFrac $ floor(x/y))*y)

squareFun p t = if round (2*t/p) `mod` 2 == 0
                        then 1/2
                        else -(1/2)

pwmFun p1 p2 t = if t `realmod` (p1+p2) < p1
                    then 1
                    else 0

sqrUWave :: (Fractional vt, UA vt) => Double -> Double -> Double -> UVecWave vt
sqrUWave period dur dt = UVecWave (toU pts) dt 0 np
    where np = round $ dur/dt
          pts = map (squareFun period . (*dt) . realToFrac) [1..np] 

pwmUWave p1 p2 dur dt = UVecWave (toU pts) dt 0 np
    where np = round $ dur/dt
          pts = map (pwmFun p1 p2 . (*dt) . realToFrac) [1..np] 

setWaveSeg w@(UVecWave arr1 dt1 off1 n1) t1 t2 val 
    = UVecWave arr dt1 off1 n1
    where (p1, p2) = (t2p w t1, t2p w t2)
          middle = replicateU (p2-p1) val
          arr = takeU p1 arr1 `appendU` middle `appendU` dropU p2 arr1
                

instance (UA a) => Wave UVecWave a where 
    at w t | t <= maxt w && t >= mint w
               = indexU (uVpnts w) (round ((t-uVoffset w)/(uVstept w)))
	   | otherwise = error "at: out of bounds access"
    shift t w = w { uVoffset= uVoffset w +t}
    stretch tfactor w = w { uVstept= tfactor*uVstept w }

instance (UA a, UA b) => MapWave UVecWave a b where
    wmapt f w = w {uVpnts = newA} 
	where newA = zipWithU (\ix v->cf ((realToFrac ix)*dt w+uVoffset w) v) (enumFromToU 0 (uVnumpnts w)) (uVpnts w)
	      cf = curry f
    wmapp f w = w {uVpnts = newA} 
	where newA = zipWithU f (enumFromToU 0 (uVnumpnts w)) (uVpnts w)
    wmap f w = w {uVpnts = mapU f (uVpnts w)} 
	      

instance (UA a) => FiniteWave UVecWave a where 
    mint w = uVoffset w
    maxt w = uVoffset w + (dt w) * (realToFrac $ uVnumpnts w -1)
    restrict w from to | from >= mint w && to <= maxt w
                           = w {uVoffset = from, 
                                uVpnts = sliceU (uVpnts w) nToDrop (nNew),
                                uVnumpnts = nNew}
                       | from < mint w = restrict w (mint w) to
                       | to > maxt w = restrict w from $ maxt w
                       | otherwise = error ("call restrict with from = "+^from++" to="+^to)

				where 	nToDrop = round ((from-(uVoffset w))/(dt w))
					nNew = round ((to-from)/(dt w))

t2p w t = round $ (t-mint w)/(dt w)


instance (UA a) => SampledWave UVecWave a where -- useful?
    dt  = uVstept 

instance (UA a) => DiscreteWave UVecWave a where 
    atpnt w ix 	|  ix >=0 && ix<(uVnumpnts w)=  indexU (uVpnts w) (ix)
                | ix>=(uVnumpnts w) = indexU (uVpnts w)(uVnumpnts w-1)
		| otherwise = error $ "atpnt: out of bounds access "++(show ix)++" for wave"
    npnts = uVnumpnts --round ((maxt w - mint w)/(dt w) )
    p2t w p = (realToFrac p)*(dt w)+ mint w
--foldlU :: UA a => (b -> a -> b) -> b -> UArr a -> b
--	where newA = zipWithU (\ix v->cf ((realToFrac ix)*dt w) v) (enumFromToU 0 (uVnumpnts w)) (uVpnts w)
--	      cf = curry f
    --foldw :: (a -> (tt,vt)-> a) -> a -> wt tt vt -> a
--    foldw f acc (RegularWave pts sdt offs npnts) = foldw' f acc (pts) (npnts) 
--        where 	foldw' f acc _ 0 = acc
              	--foldw' f acc (x:xs) p = let nacc = f acc (sdt*(realToFrac $ npnts-p)+offs, x) in
                --	                nacc `seq` foldw' f nacc xs $ p-1 
    foldw f base (UVecWave pts sdt offs npnts) 
	= foldlU (\b (ix:*:vl)-> cf b ((realToFrac ix)*sdt) vl) base $ indexedU pts
		where cf b' t v = f b' (t, v)

    pntsAsList = fromU . uVpnts
    decimate n (UVecWave pts sdt offs nps) = UVecWave newpts
                                                        (sdt*(realToFrac np1)) offs newnp
      where np1 = n+1
            newpts = (decimateUVec n pts)
            newnp = lengthU newpts -- floor $ (realToFrac nps) / (realToFrac np1)
--    detectLast valpred w = p2t w . ((uVnumpnts w)-).  fromJust $ findIndexU valpred (reverseU $ uVpnts w)
    detectLast valpred w = p2t w `fmap` findIndexUrev valpred (uVpnts w)
    detectFirst valpred w = p2t w `fmap` findIndexU valpred (uVpnts w)

--head (map fst . filterU (snd) . wmap (\(t,v)->(t:*:valpred v)) $ w)

w1 :: UVecWave Double
w1 = UVecWave (toU $ map sin [0,0.1..10]) 0.1 0 100

--pntsAsPairList w = fromU 

sumTwoWavesPnts (UVecWave arr1 dt1 off1 n1) (UVecWave arr2 dt2 off2 n2) = UVecWave (zipWithU (+) arr1 arr2) dt1 off1 n1

setWaveTo k= wmap (const k)

reverseU a = mapU (indexU a) $ enumFromThenToU (lengthU a-1) (lengthU a-2) 0

findIndexUrev pred a = let len = lengthU a -1 in
                       if len >0 then findIndexUrev' pred a (len) else Nothing
findIndexUrev' pred a 0 = Nothing
findIndexUrev' pred a pnt = if pred $ indexU a pnt
                              then Just pnt
                              else findIndexUrev' pred a (pnt-1)
                             

decimateUVec n uv = mapU (indexU uv) idxs
	where 	idxs = enumFromThenToU 0 (n+1) (lengthU uv)

--withU :: (UArr a -> UArr a) -> UVecWave a -> UVecWave a

uVecDiffs a = zipWithU (-) a $ tailU a

derivative w = w {uVpnts=mapU (/wdt) $ uVecDiffs (uVpnts w) }
               where wdt = uVstept w

sumw w = sumU $ uVpnts w
maxval w = maximumU $ uVpnts w
minval w = minimumU $ uVpnts w

maxvalt w = p2t w $ fstS (maximumByU (comparing sndS) (indexedU $ uVpnts w))
minvalt w = p2t w $ fstS (minimumByU (comparing sndS) (indexedU $ uVpnts w))



avgWavePnts :: [UVecWave Double] -> UVecWave Double
avgWavePnts (w:ws) = avgListOfWaves ws w 1

avgListOfWaves :: [UVecWave Double] -> UVecWave Double -> Int -> UVecWave Double
avgListOfWaves (w:ws) accw n = avgListOfWaves (ws) (sumTwoWavesPnts w accw) (n+1)
avgListOfWaves [] (UVecWave arr1 dt1 off1 n1) n 
    = (UVecWave arr dt1 off1 n1)
      where n' = realToFrac n
            arr = mapU (/n') arr1

mean w = sumw w / (realToFrac $ uVnumpnts w)

sdev w = recip n * sqrt (n*ex2 - ex*ex)
    where ex2 = sumU $ zipWithU (*) (uVpnts w) (uVpnts w)
          ex = sumU (uVpnts w)
          n = realToFrac $ uVnumpnts w

--localMaxAfter :: Double -> 
localMaxAfter w t = let val = w `at` t
                        w1 = restrict w t (maxt w)
                        tnextSame = fromMaybe (maxt w) $ detectFirst (<val) w1
                        w2 = restrict w t tnextSame
                        in maxval w2


convolveWithWaveAt irf w timps 
    = wmapt (\(t,v)-> v+(sum $ map (\timp-> if t>t1+timp && t < t2+timp
                                              then irf `at` (t-timp)
                                              else 0) timps)) w 
       where t1 = mint irf
             t2 = maxt irf


downSample :: Int -> UVecWave Double -> UVecWave Double
downSample n w | n > npnts w = w
               | otherwise = let npw = npnts w
                                 chunkSize = floor (npw./n)
                                 nChunks =  ceiling (npw ./ chunkSize) 
                                 narr = concatMap chunk [0..(nChunks-1)]
                                 chunk i = let arrsec = sliceU (uVpnts w) (i*chunkSize) $ min chunkSize (npw - i*chunkSize -1)
                                           in [maximumU arrsec, minimumU arrsec] 
                             in UVecWave (toU narr) ((maxt w-mint w)/2 / realToFrac nChunks) (mint w) (nChunks*2)

x ./ y = realToFrac x / realToFrac y

u2 x1 x2 = toU [x1,x2]             
