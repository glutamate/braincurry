{-# LANGUAGE NoMonomorphismRestriction, PatternSignatures, DeriveDataTypeable #-}
{-# OPTIONS -fbang-patterns -fglasgow-exts #-}

module Waves4 where

class Wave wt vt where -- wt :: * -> * -> *
    at :: (RealFrac tt, Num vt) => wt vt -> Double -> vt
--    wmap :: ((Double, vt)->vt) -> wt vt -> wt vt
    shift :: (RealFrac tt, Num vt) => Double -> wt vt -> wt vt
    stretch :: (RealFrac tt, Num vt) => Double -> wt vt -> wt vt

class MapWave wt vt1 vt2 where
    wmapp :: (Wave wt vt1,Wave wt vt2)  => (Int-> vt1->vt2) -> wt vt1 -> wt vt2
    wmapt :: (Wave wt vt1,Wave wt vt2)  => ((Double, vt1)->vt2) -> wt vt1 -> wt vt2
    wmap :: (Wave wt vt1,Wave wt vt2)  => (vt1->vt2) -> wt vt1 -> wt vt2


class Wave wt vt => FiniteWave wt vt where
    mint :: wt vt -> Double 
    maxt ::  wt vt -> Double
    restrict :: wt vt -> Double -> Double -> wt vt

class Wave wt vt => SampledWave wt vt where
    dt :: (RealFrac tt) => wt vt -> Double

class FiniteWave wt vt => DiscreteWave wt vt where
    npnts :: wt vt -> Int
    atpnt :: (Show vt ) => wt vt -> Int -> vt
    --atpnt w p = w `at` (p2t w p)
    pntsAsList :: wt vt -> [vt]
    p2t :: (Integral ix) => wt vt -> ix -> Double
    foldw :: (a -> (Double,vt)-> a) -> a -> wt vt -> a
    decimate :: Int -> wt vt -> wt vt
    detectLast :: (vt->Bool) -> wt vt ->Maybe Double
    detectFirst :: (vt->Bool) -> wt vt ->Maybe Double

combineWaves op w1 w2 = wmapt (\(t, v)-> v `op` (w2 `at` t)) w1


a +^ b =a++ show b
a ^+ b =show a++ b

instance Wave ((->) Double) a where 
    at = ($)
--    wmap f w = \t-> f (t, w t)
    shift ts f = \t-> f (t-ts)
    stretch tfactor f = \t-> f (t*tfactor)

data FunSection a = FS Double Double Double (Double->a)

instance Wave FunSection Double where 
    at (FS beg end stp f) t = f t
    shift ts (FS beg end stp f) = FS (beg-ts) (end-ts) stp $ \t-> f (t-ts)
    stretch tf (FS beg end stp f) = FS (beg*tf) (end*tf) stp $ \t-> f (t*tf)

instance FiniteWave FunSection Double where 
    mint (FS beg end stp f) = beg
    maxt (FS beg end stp f) = end
    restrict (FS beg end stp f) t1 t2 = (FS t1 t2 stp f)

instance SampledWave FunSection Double where 
    dt (FS beg end stp f) = stp

instance DiscreteWave FunSection Double where 
    npnts (FS beg end stp f) = round $ (end - beg)/stp
    pntsAsList (FS beg end stp f) = map f [beg, (beg+stp) .. end]
    p2t (FS beg end stp f) ix = (beg+(realToFrac ix)*stp)
    atpnt (FS beg end stp f) ix = f (beg+(realToFrac ix)*stp)
    decimate n (FS beg end stp f) = FS beg end (stp/(realToFrac n)) f