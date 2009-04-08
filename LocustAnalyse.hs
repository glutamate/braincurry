{-# LANGUAGE ExistentialQuantification, PatternSignatures,NoMonomorphismRestriction #-} 

module LocustAnalyse where

import NewResult
import Waves4
import SaveWaves
import Data.Maybe
import Data.List
--import Allegro.ShapeN
--import Allegro.Vector
import Query2
import Event
--import Allegro.CairoShape
import ArrayWave
import System.CPUTime
import Control.Monad
import System.Cmd(rawSystem,system)
import System.IO
--import System.Console.Readline
import ApparatusPersistence
import Database.HDBC.PostgreSQL 
import Database.HDBC 
import Charts
import Control.Concurrent
import Data.Array.Vector
import System.Directory


measureOnWave :: (Ord vt, DiscreteWave wt vt, Show vt) 
                 => wt vt -> [Double] -> (wt vt -> Double -> Maybe r) -> [r]
measureOnWave w tms fun = catMaybes $ map (fun w) tms

measureSpike :: UVecWave Double -> Double -> Maybe Spike
measureSpike w' t = let !maxv = maxval w
                        !minv = minval w
                        !maxvt = maxvalt w
                        !minvt = minvalt w
                        !samp = maxv - minv 
                        !npratio = -1*minv/maxv
                        !tpdiff = (maxvt) - (minvt) 
                        !wf = Just $ (restrict w' (maxvt-0.003) (maxvt+0.003)) {uVoffset = 0}
                        !swidth = if maxvt>minvt
                                    then let wend = restrict w maxvt (maxt w)
                                             winit = restrict w (mint w) minvt
                                             !tend = detectFirst (not . (>0.2*maxv)) wend
                                             !tbeg = detectLast (not . (<0.2*minv)) wend
                                         in liftM2 (-) tend tbeg
                                    else let wend = restrict w minvt (maxt w)
                                             winit = restrict w (mint w) maxvt
                                             !tend = detectFirst (not . (<0.2*maxv)) wend
                                             !tbeg = detectLast (not . (>0.2*minv)) wend
                                         in liftM2 (-) tend tbeg
                        !s = case swidth of 
                               Just sw -> Just $! Spike samp npratio sw tpdiff Nothing maxvt wf
                               Nothing -> Nothing in                    
                    s
    where w :: UVecWave Double
          w = restrict w' (t-0.003) (t+0.003) 

--spikeVec :: Spike -> Vector Four Double
--spikeVec (Spike samp npratio swidth tpdiff _ _) = vec4 samp npratio swidth tpdiff
                               
--points :: [(Double,Double)]-> [Shape Two Double]
--points = map (\(x,y)->Translate (vec2 x y) $ circles 0.0001)

--addAxes :: [Shape Two Double] -> [Shape Two Double]

--main = analyseSessn 35
{---main = setSpikeCriteriaSession 39
--anal = return () {- do trspks <- getTrialsWavesAndSpikes $ InSession 39 Trials
          let allSpikes = concat $map trd3 trspks
          --let avgSpikeWf = avgWavePnts $ catMaybes $ map waveForm spks
          let wtr1 = snd3 $ head trspks
          --let instRates =avgWavePnts $ map trInstRate trspks
          --plotRegWgif avgSpikeWf "spike_waveform.gif"
          --print $ filter ((>5.02) .&&. (<5.07)) $ map tspike $ trd3 $ head trspks
          --plotRegWgif gausssec "normal.gif"
          print $ length allSpikes
          --plotWaves (catMaybes $ map waveForm allSpikes)
          --plotRegW (restrict wtr1 0 6  ) -- "trial1.gif"
          plotWaveAndEvents (restrict wtr1 4.8 5.25) (map tspike. trd3 $ head trspks)

          --plotRegW (restrict instRates 4 6 ) -- "inst_rates.gif"
          --print instRate1
         -- plotPntsGif (map (\s->(amp s, tPeakDiff s)) allSpikes) "spike_amp_tpeak.gif"
-} -}

analyseSessn sn = do
  -- plot all spikes
  spkWaveforms (inSess sn) >>= plotAs "allSpikes"
  -- mean inst rate of 1st pulse
{-
  let lovs = [5,10,20]
  let colours = [Red, Blue,Black]
  meanInstHzWs <- forM lovs instRateForLoV 
  plotAs "meanInstRates" $ zip meanInstHzWs colours
  forM_ (zip lovs (map maxval meanInstHzWs)) $ \(lov, p1hz)-> do
                    isiWs <- forM [30,60,120] $ peakRatesForLovISI lov p1hz
                    plotAs ("habit_lov_"++show lov) $ isiWs `zip` colours
  -}                           
    where plotAs s = plotGif (s++"_sessn"++show sn++".gif")
          instRateForLoV lov = meanInstRate (wLov lov $ wPulseNum 1 $ inSess sn)
          peakRatesForLovISI lov p1hz isi = do 
                               peakHz <- forM [2..10] $ 
                                         \pulse-> maxval `fmap` meanInstRate (wLov lov $ wISI isi $ wPulseNum pulse $ inSess sn)
                               --print  $"lov, isi="++show (lov, isi)
                               --print peakHz
                               return $ wvFromNumList 1 (p1hz:peakHz)
avgInstRateInSessns :: [Int] -> IO [(UVecWave Double, Col)]
avgInstRateInSessns sessns = do
  let lovs = [5,10,20]
  wsl <- forM lovs $ \lov-> do ws <- forM sessns $ \sn-> meanInstRate (wLov lov $ wPulseNum 1 $ inSess sn)
                               return $ avgWavePnts ws
  return $ zip wsl [Red, Blue,Black]


somew = (snd3 . head) `fmap` getTrialsWavesAndSpikes (inSess 39)


allFigs = do sweep (inSess 39) 1 0 6 >>= plotGif "fig1.gif"
             sweep (inSess 39) 1 4.8 5.2 >>=plotGif "fig2.gif"
             sweep (inSess 39) 2 4.8 5.2 >>=plotGif "fig2b.gif"

getTrialsWavesAndSpikes :: Query [TrialInfo] -> IO [(TrialInfo, UVecWave Double, [Spike])]
getTrialsWavesAndSpikes q = do
  (Just trs) <- ask $ q
  retrieveStoredSpikes (trs)


(<*>) = flip fmap


plotPerTrial :: ((TrialInfo, UVecWave Double, [Spike]) -> Double) -> Query [TrialInfo] -> IO ()
plotPerTrial f q = do
  (Just trs) <- ask $ q
  trws <- retrieveStoredSpikes (trs)
  plotPnts $ zip (map (trialId . fst3) trws) (map f trws)

maxVoverSess s = plotPerTrial trMaxV (inSess s) 


trWSpk = getTrialsWavesAndSpikes . inSess
          
inSess n = InSession n Trials

wSpec s = SpecLike s

app n = SpecLike $ "TrialParameter \"approachNum\" "++show (isInt n)

isInt :: Int -> Int
isInt = id

sound = SpecLike "TrialName \"sound\""

noSound = SpecLike "TrialName \"nosound\""

wLov :: Int -> Query [TrialInfo] -> Query [TrialInfo]
wLov 5 = wSpec "Const 5.0e-3"
wLov 10= wSpec "Const 1.0e-2"
wLov 20 = wSpec "Const 2.0e-2"
wLov 40 = wSpec "Const 4.0e-2"

wNamedLov :: Int -> Query [TrialInfo] -> Query [TrialInfo]
wNamedLov 5 = wSpec $ "\"approachLoV\" 5.0e-3"
wNamedLov 10 = wSpec $ "\"approachLoV\" 1.0e-2"
wNamedLov 20 = wSpec $ "\"approachLoV\" 2.0e-2"
wNamedLov 40 = wSpec $ "\"approachLoV\" 4.0e-2"


wISI :: Int -> Query [TrialInfo] -> Query [TrialInfo]
wISI isi = wSpec ("LeastWaitBefore "++show isi++".0") -- 

wPulseSepSecs :: Int -> Query [TrialInfo] -> Query [TrialInfo]
wPulseSepSecs isi = wSpec $ "\"pulseSepSecs\" "++show isi++".0"

wPulseNum :: Int -> Query [TrialInfo] -> Query [TrialInfo]
wPulseNum pn = wSpec $ "\"approachNum\" "++show pn

wSoundAmp mvInt = wSpec $ "\"soundAmpmV\" "++show mvInt

--meanInstRate :: Query [TrialInfo] -> IO ()
meanInstRate q = do trspks <- filter (not . null . trd3) `fmap` (getTrialsWavesAndSpikes q)
                    let instRates =avgWavePnts $ map trInstRate trspks
                    return (restrict instRates 4 5.5 ) -- "inst_rates.gif"

numSpikes :: (Fractional b) => Query [TrialInfo] -> IO b
numSpikes q = do trspks <- filter (not . null . trd3) `fmap` (getTrialsWavesAndSpikes q)
                 let ntrials = fromIntegral $ length trspks
                 return $ sum (map (fromIntegral . length . trd3) trspks) / ntrials

spkWaveforms q = do trspks <-  getTrialsWavesAndSpikes q 
                    let allSpikes = concat $map trd3 trspks
                    return (catMaybes $ map waveForm allSpikes)
                    
plotSweep sn trn t1 t2 = do x <- (fst `fmap` sweep (inSess sn) trn t1 t2) 
                            forkIO $ plotGraph x
plotSweep' sn trn t1 t2 = do x <- (sweep (inSess sn) trn t1 t2) 
                             plot x

sweep q trn t1 t2 = do  
  trspks <- getTrialsWavesAndSpikes q
  let swep = snd3 $ trspks!!trn
  let stimes = (map tspike. trd3 $ trspks!!trn)
  --plotRegW swep
  return (restrict swep t1 t2, stimes)

sweepId n = do (Just w) <- ask $ InTrial n $ waves "ecVoltage"
               return w

maxRateOverTime q = do
  trspks' <- getTrialsWavesAndSpikes q
  let trspks = filter (not . null . trd3) trspks'
  let peakHzs = map (maxval . trInstRate) trspks
  plotPnts $ zip (map (trialId . fst3) trspks) peakHzs

trInstRate (tr, w, spks) 
    = convolveWithWaveAt (gausssec) 
                         (setWaveTo (0::Double) w) 
                         $ map tspike spks
trMaxV (tr,w,spks)
    = maxval w-minval w


fst3 (x,_,_)=x
snd3 (_,x,_)=x
trd3 (_,_,x)=x

--x>-y = (x>(negate y))
--x-y = (x>(negate y))

pairWithM :: Monad m =>(a-> m b) -> [a]-> m [(a,b)]
pairWithM f xs = mapM (\x->do fx <- f x; return (x,fx)) xs

--newtype Filtr a = Filtr (forall b. (a->b, b-> Bool))

data Filtr a = forall b. Filtr (a->b) (b->Bool)
x ~> y = Filtr x y

infixl 5 .>., .<.

x .>. y = Filtr x (>y) 
x .<. y = Filtr x (<y) 

filterBy :: [Filtr a] -> [a] -> [a]
filterBy fltrs xs = filter (\x->and $ map (`filter1` x) fltrs) xs
    where filter1 (Filtr conv tst) x = (tst . conv) x                           

getTrialWave tr = do  (Just w) <- ask $ InTrial (trialId tr) $ waves "ecVoltage"
                      return w

storeSpikesAsResult trwspks = do
  forM_ trwspks $ \(tr, w, spks)-> forM_ spks $ \(Spike a npr w tpd _ tsp _) -> do
                                     run unsafeConn sqlStr [toSql a, toSql npr, toSql w,toSql tpd, toSql tsp, toSql $trialId tr]
      where sqlStr = "insert into result_spike (amplitude,\"negPosRatio\",width,\"tPeakDiff\",tspike, result_id) values (?, ?, ?, ?, ?, ?)"

retrieveStoredSpikes :: [TrialInfo] -> IO [(TrialInfo, UVecWave Double, [Spike])]
retrieveStoredSpikes trs = do
  forM trs $ \tr->do spks <- getSpikesForTrial $trialId tr
                     w <- getTrialWave tr
                     return (tr, w, map (setSpkWfFromW w) spks) 

setSpkWfFromW w spk = spk { waveForm = Just wf }
                                where tsp = tspike spk
                                      wf = (restrict w (tsp-0.003) (tsp+0.003)) {uVoffset = 0}
getSpikesForTrial :: Int -> IO [Spike]
getSpikesForTrial trId 
    = do qres <- quickQuery' unsafeConn sqlStr [toSql trId]
         catMaybes `fmap` mapM parseSql qres            
    where sqlStr = "SELECT amplitude,\"negPosRatio\",width,\"tPeakDiff\",\"clusterNum\", tspike from result_spike where result_id =? order by id"

getSpec tr
    = do ((SqlString t:_):_) <- quickQuery' unsafeConn sqlStr [toSql tr]
         mapM_ putStrLn $ splitOn (==',') t
    where sqlStr = "SELECT spec from trial where id =?"

shortSpec spec
    = map chop . filter accept . splitOn (==',') $ spec
    where accept s | "[Tr" `isPrefixOf` s = True
                   | "Tr" `isPrefixOf` s = True
                   | otherwise = False
          chop s = (unwords . tail $ words s)++", "

listSessTrials sess = do Just trs <- ask $ InSession sess Trials
                         forM_ trs $ \trinf -> do
                           let tm = (!!1). words  $ triggerTime trinf 
                           putStr $ show $ trialId trinf
                           putStr $" ("++tm++"): "
                           print . shortSpec $ trialSpec trinf
 
                  
askSessions  =ask Sessions         
    

                          
applySpikeFilter amin amax tpmin tpmax spks 
    = nubBy (\s1 s2 -> abs(tspike s2-tspike s1) < 0.001) . filterBy [
       tPeakDiff .>. tpmin
      , tPeakDiff .<. tpmax
      , amp .>. amin
      , amp .<. amax
      ] $ spks

getSpksInWave w tr = do
  let spks = getPutativeSpikes w 
  (mtpdMax) <- getAnalysisParameter "tpeakdiff_max" (trialId tr)
  (mtpdMin) <- getAnalysisParameter "tpeakdiff_min" (trialId tr)
  (mampMin) <- getAnalysisParameter "amp_min" (trialId tr)
  (mampMax) <- getAnalysisParameter "amp_max" (trialId tr)
  if and [isJust mtpdMax, isJust mtpdMin, isJust mampMin,isJust mampMax]
     then return $ applySpikeFilter (fJ mampMin) (fJ mampMax) (fJ mtpdMin) (fJ mtpdMax) spks
     else return []
      where fJ = fromJust
getPutativeSpikes w = let putativeSpkTms = map (etime) . snd $ spikeD 0.05 (w,[]) in
                      measureOnWave w putativeSpkTms measureSpike

setSpikeCriteriaSession sessn ndivs = do
  (Just trs) <- ask $ InSession sessn Trials
  let ntrials = length trs
  --let idFirst = trialId $ head trs
  --let idLast = trialId $ last trs
  --print $ "ntrials="++^ntrials
  --(ndivs::Int) <- askFor "ndivisions"
  let trlsts = splitListIntoSegs (ntrials `div` ndivs) trs
  mapM_ (\trs-> do
           trws <- mapM (\tr->do w <- getTrialWave tr
                                 return (tr,w)) trs
           handleSqlError $ setSpikeCriteria trws
       ) trlsts
  commit unsafeConn

splitListIntoSegs _ [] = []
splitListIntoSegs elPerSeg xs = let (l1, l2) = splitAt elPerSeg xs in
                                l1 : splitListIntoSegs elPerSeg l2

getTrWaves sess = do
  (Just trs) <- ask $ InSession sess Trials
  mapM (\tr->do w <- getTrialWave tr
                return (tr,w)) trs

exportToIgor sess = do
  d1 <- getCurrentDirectory
  trws <- getTrWaves sess
  createDirectory dir
  setCurrentDirectory dir 
  forM_ trws $ \(tr,w)-> do
                      let tid = show $ trialId tr
                      saveAsItx ("ecVoltage_"++tid) w
  setCurrentDirectory d1 
      where dir = "/home/tomn/waves/sess"++show sess

idBetween _ _ [] = []
idBetween x y (trw:trws) = let tid = trialId . fst $ trw in 
                           if tid >= x && tid <=y
                              then trw:idBetween x y (trws)
                              else if tid >y then [] else idBetween x y (trws)

dispVert trws = 
  let ws = map snd trws
      amps = map (\w-> maxval w - minval w) ws
      maxamp = min 1.5 $ foldl1 max amps 
      mytrws = zipWith (\(tr,w) nc -> (tr, wmap (+(nc*maxamp)) w)) trws [0..] in
  do  mapM_ (\(tr,w)-> print $ trialId tr) mytrws
      plot $ map (downSample 500 . snd) $ mytrws

wThresh th trws = 
  let ws = map snd trws
      amps = map (\w-> maxval w - minval w) ws
      maxamp = min 1.5 $ foldl1 max amps in
  do let newtrws = zipWith (\(tr,w) nc -> (tr, wmap (+(nc*maxamp)) w)) trws [0..]
     let thamps = map (\nc->maxamp*nc+th) [0..]
     let thws = map (\a->UVecWave (toU [a,a]) 6 0 2) thamps
     let plotws =(map (downSample 1000 . snd) newtrws) 
     plot $ plotws ++ take (length plotws) thws


--commit' = commit unsafeConn

setThresh th trws = do
  let tr1 = fst $ head trws
  let tr2 = fst $ last trws
  let setParam pnm vl = setAnalysisParameter pnm (trialId tr1) (trialId tr2) vl
  run unsafeConn ("DELETE from analysis_parameter where trial_id_start>=? AND trial_id_stop<=? AND name = 'amp_th'") 
                       [toSql . trialId $ tr1, toSql . trialId $ tr2]
  setParam "amp_th" th
  -- delete existing spikes
  run unsafeConn sqlStr [toSql $trialId tr1, toSql $trialId tr2 ]
 
  -- measure new spikes
  let trwspks = map (\(tr, w) -> (tr,w,measureOnWave w (simpleDetector w th 0.002) measureSpike)) trws
  forM trwspks $ \(tr,w,spks) -> print spks
  -- store as result 
  storeSpikesAsResult trwspks
  commit unsafeConn
    where sqlStr = "delete from result_spike where result_id>=? AND result_id<=?"
 
simpleDetector w th refr = case detectFirst (pred) w of
                             Just t -> if (t+refr)>lastt 
                                         then [t] 
                                         else t : simpleDetector (restrict w (t+refr) (lastt)) th refr
                             Nothing -> []
    where lastt = maxt w
          pred = if th > 0 then (>th) else (<th)

--  getTrWaves 69 <*> idBetween 9200 9300 >>= wThresh 5 0.4

someOf :: Int -> [(TrialInfo, UVecWave Double)] ->[(TrialInfo, UVecWave Double)]
someOf n trws' =  
  let ntotal = length trws'
      nperjump = ntotal `div` (n-1)
  in (everyNth nperjump trws')++[last trws']

  
everyNth _ [] = []  
everyNth n (x:xs) = x : (everyNth n $ drop (n-1) xs)



--getTrWaves 69 <*> idBetween 9103 9116 >>= setSpikeCriteria

setSpikeCriteria trws = do
  let tr1 = fst $ head trws
  let tr2 = fst $ last trws
  let trwspks = map (\(tr,w) -> (tr, w, getPutativeSpikes w)) trws
  let allspks = concat $ map trd3 trwspks
  run unsafeConn ("DELETE from analysis_parameter where trial_id_start>=? AND trial_id_stop<=?") 
                       [toSql . trialId $ tr1, toSql . trialId $ tr2] 
  lims <- getLimitsFromGnuplot (map (\s->(amp s, tPeakDiff s)) allspks)
  print lims
  let (amin:amax:tpmin:tpmax:_) = lims
  let setParam pnm vl = setAnalysisParameter pnm (trialId tr1) (trialId tr2) vl
  setParam "amp_min" $ amin
  setParam "amp_max" $ amax
  setParam "tpeakdiff_min" $ tpmin
  setParam "tpeakdiff_max" $ tpmax
  mapM_ (\(tr, w, spks) -> do 
           let okSpks = applySpikeFilter amin amax tpmin tpmax spks 
           --mapM_ (store unsafeConn (toInteger $ trialId tr) "spike") okSpks
           storeSpikesAsResult [(tr, w, okSpks)]
        ) trwspks
      
  commit unsafeConn


s ++^ s' = s ++(show s')

{-askForDbl :: String -> IO Double
askForDbl=askFor

askFor s = do ln <- readline $ s++"> "
              --print ln
              return . read . fromJust $ ln
-}
                            
  

gauss mean sd x = exp(-(x-mean)**2/(2*sd**2))/(sd*sqrt(2*pi))

gausssec = FS (-0.1) (0.1) (5.0e-5) (gauss 0 0.0125)

stochFit :: (Ord b, Monad m) => (a->b) -> (a-> m a) -> Int -> a -> m a
stochFit fitfun genNew gens cur = stochFit' fitfun genNew gens cur (fitfun cur)

stochFit' _ _ 0 val _ = return val
stochFit' fitfun genNew gens cur curFit 
    = do new <- genNew cur
         let newFit = fitfun new
         if  newFit < curFit
            then stochFit' fitfun genNew (gens-1) new newFit
            else stochFit' fitfun genNew (gens-1) cur curFit 

--to do: 

-- missing spikes
-- drift


{-



-}

