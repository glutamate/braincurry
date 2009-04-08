{-# OPTIONS -fbang-patterns #-}

module SolveODE where

import Waves2
import GNUPlot
--import Numeric.Units.Dimensional.Prelude
--import Estimate hiding (Estimate)
--import qualified Prelude as P
--import qualified Estimate as E


{- moved to SimNeuron.hs

solveNeuron n iw delt tmax = intStep 0 (vrest n)
    where intStep !t !v = solveEulerCPS (neuronDV n iw) v t delt k
          k !t' !v' | t' >= tmax = [] -- done
                    | v' > vth n = v':0.02:(replicate refracSteps $ vrest n)++(intStep (t'+3*delt) (vrest n)) --spike
                    | otherwise = v' : (intStep (t'+delt) v')
	  refracSteps = round ((refrac n)/delt)


data Neuron a = Neuron { rin :: a, cm :: a, vrest :: a, vth :: a, refrac :: a}
neuronDV (Neuron r c vr vth ref) iw t v = ((iw `at` t) -(v-vr)/r)/c

-}

solveEulerCPS df y t delt kont = kont t (y + delt * (df t y))
solveEulerCPS' df y t delt kont = let ~(d, s) = df t y in kont s t (y + delt * d)

solveRK2CPS df y t delt kont = kont t (y + k2)
	where	k1 = delt * (df t y)
                k2 = delt * (df (t+delt/2) (y+k1/2))

solveRK4CPS df y t delt kont = kont t (y + k1/6 + k2/3 + k3/3 + k4/6)
	where	k1 = delt * (df t y)
                k2 = delt * (df (t+delt/2) (y+k1/2))
                k3 = delt * (df (t+delt/2) (y+k2/2))
                k4 = delt * (df (t+delt) (y+k3))

--solveEuler :: (RealFrac tt, Num vt, Ord vt, Mul tt dvt vt) => (tt -> vt -> dvt) -> vt -> tt -> (tt -> vt -> Bool) -> RegularWave tt vt

solveEuler df y0 delt stopcond  = soln
    where soln =  solve y0 0
          solve y npnt | stopcond t y = []
                       | otherwise = yn : solve yn (npnt+1)
                       where t = delt*(fromInteger npnt)
                             yn = y + delt * (df t y)

solveRK2 df y0 delt stopcond = RegularWave { pnts = soln,
                                           stept = delt,
                                           offset = 0,
                                           numpnts = length soln }
    where soln =  solve y0 0
          solve y npnt | stopcond t y= []
                       | otherwise = yn : solve yn (npnt+1)
                       where t = delt*(fromInteger npnt)
                             k1 = delt * (df t y)
                             k2 = delt * (df (t+delt/2) (y+k1/2))
                             yn = y + k2

solveRK4 df y0 delt stopcond = RegularWave { pnts = soln,
                                           stept = delt,
                                           offset = 0,
                                           numpnts = length soln }
    where soln =  solve y0 0
          solve y npnt | stopcond t y= []
                       | otherwise = yn : solve yn (npnt+1)
                       where t = delt*(fromInteger npnt)
                             k1 = delt * (df t y)
                             k2 = delt * (df (t+delt/2) (y+k1/2))
                             k3 = delt * (df (t+delt/2) (y+k2/2))
                             k4 = delt * (df (t+delt) (y+k3))
                             yn = y + k1/6 + k2/3 + k3/3 + k4/6


--testing 

--intFire r c vrest vth gampaw = foo
--    where voltDerivative t v 
--              = ((gampaw `at` t) * r - v)/( r  * c )
