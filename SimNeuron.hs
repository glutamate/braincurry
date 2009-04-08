{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS -fno-monomorphism-restriction #-}
module SimNeuron where

import SolveODE
import Waves4
--import GNUPlot
import System.IO.Unsafe
import System.Random
import Data.Maybe

db :: Show a => String -> a -> b -> b 
db s x y = unsafePerformIO $ putStrLn (s++(show x)) >> return y 
{-intFire (IFNeuron vth vreset refrac comp) delt tmax = intStep 0 (vreset)
    where intStep t v = solveEulerCPS (cmptmntDV comp) v t delt k
          k t' v' | t' >= tmax = [] -- done
                  | v' > vth = v':0.02:(replicate refracSteps $ vreset)++
                                       (intStep (t'+3*delt) (vreset)) --spike
                  | otherwise = v' : (intStep (t'+delt) v')
	  refracSteps = round ((refrac)/delt)

type Current a = a -> a -> a
-}
newtype Current a = Current { unCurrent :: (a -> a -> (a, Current a)) }

--data Compartment a = Cmp a [Current a] -- (Capacitance, Currents)
data Compartment a = Cmp a [Current a] -- (Capacitance, Currents)

--cmptmntDV :: Compartment vt -> (tt -> vt -> vt)
--cmptmntDV :: RealFrac a => Compartment a -> (a -> a -> a)
--cmptmntDV (Cmp cm is) t v = (/cm) . sum . map (\i-> i t v) $ is -- ((iw `at` t) -(v-vr)/r)/cm

--cmptmntDV :: Compartment vt -> (tt -> vt -> vt)
cmptmntDV :: RealFrac a => Compartment a -> (a -> a -> (a,Compartment a))
cmptmntDV (Cmp cm is) t v = ((/cm) . sum . map fst $ currents, Cmp cm $ map snd currents) -- ((iw `at` t) -(v-vr)/r)/cm
    where currents = map (\i-> unCurrent i t v) $ is

--intFire' :: RealFloat a =>a-> a->a->Compartment' a->a->a->[a]
intFire (IFNeuron vth vreset refrac comp) delt tmax = intStep 0 (vreset) comp
    where   intStep t v thisCmp = solveEulerCPS' (cmptmntDV thisCmp) v t delt k 
            --deriv cmp = \t v -> let calcDeriv = (cmptmntDV' comp) t v in fst calcDeriv
            k nxCmp t' v' | t' >= tmax = [] -- done
                          | v' > vth = v':0.02:(replicate refracSteps $ vreset)++
                                           (intStep (t'+3*delt) (vreset) nxCmp) --spike
                          | otherwise = v' : (intStep (t'+delt) v' nxCmp)
            refracSteps = round ((refrac)/delt)

--rIn r e _ v = (e-v)/r -- neg of v-e...
--iInj wf t v = wf `at` t

rIn:: RealFloat a =>a-> a-> Current a
--rIn' r e = rInC 
--    where rInC = Current' (\_ v-> ((e-v)/r,rInC))  -- neg of v-e...

simplCurrent:: RealFloat a =>(a->a->a)-> Current a
simplCurrent f = sC 
    where sC = Current (\t v-> (f t v,sC))

streamCurrent :: RealFloat a =>a->[a]-> Current a
streamCurrent delt xss = strmC xss 0 
    where strmC xs last = Current (\t _-> if t>last+delt 
                                             then let xs' = tail xs in (head xs',strmC (tail xs') t)
                                             else (head xs,strmC xs last))

streamCurrent' :: RealFloat a =>a->[a]-> Current a
streamCurrent' delt xss = strmC xss 
    where strmC xs = Current (\t _-> (head xs,strmC (tail xs) ))

--poissonSynapse :: Monad m => RealFloat a =>a->[a]-> m Current a
--poissonSynapse gt erev pOfT = strmC xss 
--    where strmC xs = Current (\t _-> (head xs,strmC (tail xs) ))

--poissonSynapse :: Monad m => RealFloat a =>a->[a]-> m Current a
--poissonSynapse gt erev pOfT = strmC xss 
--    where strmC xs = Current (\t _-> (head xs,strmC (tail xs) ))

type Plasticity' s = ((Double,s)-> (Double, s) ,s)
--poissonSynapse ::
{-poissonSynapse gt erev pOfT (plastf, initps) = pSyn 0 initps []
    where pSyn tlast ps spks = Current (\t v->  let r = (t-tlast)*pOfT(t) - (unsfRndR (0,1))
                                                    (ampNewNow, ps') = plastf (t,ps)
                                                    (newSpks,ps'') = if r>0
                                                                        then ((t,ampNewNow,0):spks, ps')
                                                                        else (spks,ps)
                                                    filtSpks = filter countSpk . map updSpk $ newSpks
                                                    updSpk (tspk,amp,_)=(tspk,amp, amp * (gt `at` (t-tspk)))
                                                    countSpk (tspk,amp,ampNow) = t-tspk>0.01 && ampNow <0.01
                                                    sumI = (sum . map trd3 $ filtSpks) * (erev - v) in
                                                (sumI, pSyn t ps'' newSpks)) -}
synapse gt erev spkTms (plastf, initps) = syn initps [] spkTms
    where syn ps spkAmps spks 
            = Current (\t v->  let (incl, notYet) = span (<t) spks
                                   (inclAmps, ps') = loopf plastf ps incl
                                   countSpk (tspk,_,ampNow) = (t-tspk)<0.01 || ampNow>0.01
                                   updSpk (tspk,amp,_)=(tspk,amp, amp * (gt `at` (t-tspk)))
                                   nSpkAmps = map updSpk $ (zip3 incl inclAmps (repeat 1))++(filter countSpk spkAmps)
                                   trd3 (_,_,z)=z
                                   sumI = (sum . map trd3 $ nSpkAmps) * (erev - v) in
                               (sumI, syn ps' nSpkAmps notYet))

loopf :: ((a,b)->(c,b))-> b-> [a]-> ([c],b)
loopf f b as = loopf' f b as []

loopf' f b [] acc = (reverse acc,b)
loopf' f b (a:as) acc = let (c,b') = f (a,b) in
                        loopf' f b' as (c:acc)


poissonSpikes pOfT delt rnds tend = spks
    where   tms =[0, delt..tend]
            spks = map fst . filter (\(t,r)->r<delt*pOfT t) $ zip tms rnds

{-poissonSpikes' pOfT delt rnds = spks
    where   tms = map (*delt) [0..]
            spks = mapMaybe (spkTm) $ zip tms rnds
            spkTm (t,r) = let ratio = r/(pOfT t) in
                          if ratio <1
                            then Just t+ratio*delt
                            else Nothing-}


noPlast' = (\(t,_)->(1,()), ())


unsfRnd = unsafePerformIO randomIO
unsfRndR = unsafePerformIO . randomRIO

iInj wf = simplCurrent (\t _-> wf `at` t)

rIn r e = simplCurrent (\_ v->(e-v)/r) -- neg of v-e...

simpleSyn gt eRev spkTms = simplCurrent (simpleSyn' gt eRev spkTms)

--simpleSyn' :: RealFrac a => (a -> a) -> a -> [(a, a)] -> (a -> a -> a) --Current a
simpleSyn' gt eRev spkTms t v = (synFs t)*(eRev-v)
    where   synFs = foldr (!+) (const 0) . map (\(t,amp)-> (*amp) . shift t gt) $ spkTms
            (!+) = \f g t -> f t + g t

grCcomp:: Compartment Double
grCcomp = Cmp 50e-12 [rIn 1e9 (-0.08)]

grcNeuron = IFNeuron (-0.05) (-0.08) (0.002) grCcomp

class AddCurrent n where
	(.+.) :: n a -> Current a -> n a

instance AddCurrent Compartment where
	(Cmp c is) .+. i = Cmp c (i:is)

instance AddCurrent IntFireNeuron where
	ifn .+. i = ifn { compartment = compartment ifn .+. i }

--(.+.) :: Compartment a -> Current a -> Compartment a
--(c, is) .+. i = (c,i:is)

data IntFireNeuron a = IFNeuron { 	vth::a, vreset::a, refrac::a, 
					compartment :: Compartment a
				}


--simpleSyn :: (tt -> vt) -> vt -> [(tt, vt)] -> Current vt

fire500hz = [0.01, 0.012..0.09]

applyPlasticity :: Plasticity -> [Double] -> [(Double,Double)]
applyPlasticity plast tms = zip tms (plast tms)

--tsGrC i = intFire (grcNeuron .+. iInj (setAmp (i*1e-12) 0.02) ) 0.0001 0.1
--tsGrcS i = intFire (grcNeuron .+. simpleSyn (alphaGmax (i*1e-12) 800) 0 fire500hz) 0.0001 0.1


--liftOp :: (a -> b -> c) -> (a -> c) -> (b -> c) -> a -> b -> c
--liftOp op f g x y = 

--from some NEURON documentation
--alpha tau t | t<=0 = 0
--            | otherwise = (t/tau)*exp ((t-tau)/tau)

--Rusakov 2001 biophysj 
alpha tau t | t<=0 = 0
            | otherwise = (tau^2*t)*exp ((-tau)*t)


alphaGmax g tau t = g*(alpha tau t)

mySyn a i = map (alphaGmax (i*1e-12) a) [0, 0.0001..0.01]

--amplify gain = wmap (\(_,v)-> gain * v) 



setAmp amp dur t | t>dur && t<2*dur = amp
                 | otherwise = 0

step amp dur t | t>0 && t<dur = amp
               | otherwise = 0

iinj = 31e-12


--tsGrC t = solveNeuron grC (setAmp iinj t) 0.0001 (3*t)
--tsGrC0 = solveNeuron grC (const 0) 0.0001 0.06

--myPl ts= plotLists [] [ts]

type Plasticity = [Double] -> [Double]  -- spik times to rel efficacy

noPlast :: Plasticity
noPlast tms = map (const 1.0) tms

markramTsodyks trec use ase (t1:tms) = 1:mtIter tms 1 t1
	where 	mtIter [] eplast tlast = []
		mtIter (t:tms) eplast tlast = epnext:mtIter tms epnext t
			where 	epnext = eplast*(1-use)*expterm+ ase*use*(1-expterm)
				expterm= exp(-(t-tlast)/trec)






