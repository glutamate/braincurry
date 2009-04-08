{-# LANGUAGE NoMonomorphismRestriction, PatternSignatures, FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS -fbang-patterns #-}


module SaveWaves where

import Data.Binary
--import Codec.Compression.GZip
import Unsafe.Coerce
import qualified Data.ByteString.Lazy as L
import Waves4
import ArrayWave
import Data.Array.Vector

import System.Cmd(rawSystem,system)
import Data.Unique
import Data.List
import Control.Monad
import System.IO


--import GNUPlot

saveWave :: Binary w => FilePath-> w -> IO ()
saveWave fp w = L.writeFile fp {-. compress-} . encode $ w --writeFile fp . show

loadWave :: Binary w =>FilePath-> IO (w)
loadWave fp = return . decode {-. decompress -}=<< L.readFile fp--readFile fp >>= return . read


saveAsItx wn w 
    = withFile (wn++".itx") WriteMode $ \h-> do
        hPutStrLn h "IGOR"
        hPutStrLn h $ "WAVES\t"++wn
        hPutStrLn h "BEGIN"
        forM_ (fromU . uVpnts $ w) $ hPutStrLn h . ('\t':) . show
        hPutStrLn h "END"
        hPutStrLn h $ "X SetScale/P x "++(show $ mint w)++","++(show $ dt w)++",\"\", "++wn++";"
        return ()

saveXYAsItx wn pts
    = withFile (wn++".itx") WriteMode $ \h-> do
        hPutStrLn h "IGOR"
        hPutStrLn h $ "WAVES\t"++wn++"_x\t"++wn++"_y"
        hPutStrLn h "BEGIN"
        forM_ (pts) $ \(x,y) -> hPutStrLn h $ "\t"++^x++"\t"++^y
        hPutStrLn h "END"
        -- hPutStrLn h $ "X SetScale/P x ,"++(show $ dt w)++",\"\", "++wn++";"
        return ()
      where a ++^ b = a ++ (show b)
            wy = map snd pts

  
--loadTextWave :: FilePath-> IO (RegularWave Double)
--loadTextWave fp = readFile fp >>= return . read

wvFromNumList :: Double -> [Double] -> UVecWave Double
wvFromNumList sdt lst = UVecWave (toU lst) sdt 0 (length lst)


{-tsLoadSave = do w48 <- loadTextWave "48_ecVoltage.twv"
		saveWave "48_ecVoltage.bwv" w48 
		w2 <- loadWave "48_ecVoltage.bwv" 
		--encodeFile "pi.bindbl" (2.54::Double)
		print $ length $ pnts w2
		print $ length $ pnts w48
		print $ mean w2
		print $ mean w48
-}
{-instance Binary (RegularWave Double) where
    put (RegularWave pts st ofs len) 
      =  do put st 
            put ofs 
            put len
            mapM_ (put . toWord) pts
	where 	toWord p = ((unsafeCoerce p) :: Word64)

    get = do  st <- get
              ofs <- get
              len <- get
              pts <- mapM (const get) [1..len]
              return (RegularWave (map fromWord pts) st ofs len)
	where fromWord (w:: Word64) = unsafeCoerce w
-}
instance Binary (UVecWave Double) where
    put (UVecWave pts st ofs len) 
      =  do put st 
            put ofs 
            put len
            mapM_ (put . toWord) $ fromU pts
	where 	toWord p = ((unsafeCoerce p) :: Word64)

    get = do  st <- get
              ofs <- get
              len <- get
              pts <- mapM (const get) [1..len]
              return (UVecWave (toU (map fromWord pts)) st ofs len)
	where fromWord (w:: Word64) = unsafeCoerce w

{-
instance Binary (RegularWave Float) where
    put (RegularWave pts st ofs len) 
      =  do put st 
            put ofs 
            put len
            mapM_ (put . toWord) pts
	where 	toWord p = ((unsafeCoerce p) :: Word32)

    get = do  st <- get
              ofs <- get
              len <- get
              pts <- mapM (const get) [1..len]
              return (RegularWave (map fromWord pts) st ofs len)
	where fromWord (w:: Word32) = unsafeCoerce w

-}

data Col = Black | Green | Blue | Red 

showGnuPlotCol Black = "lt rgb \"black\""
showGnuPlotCol Green = "lt rgb \"green\""
showGnuPlotCol Blue = "lt rgb \"blue\""
showGnuPlotCol Red = "lt rgb \"red\""

class Plottable a where
    plot :: a -> IO ()
    plotGif :: String -> a -> IO ()

instance Plottable (UVecWave Double) where
    plot = plotRegW
    plotGif = flip plotRegWgif

instance Plottable [UVecWave Double] where
    plot ws = plotWaves (zip ws $ repeat Red)
    plotGif fn ws = plotWavesGif (zip ws $ repeat Red ) fn

instance Plottable [(UVecWave Double, Col)] where
    plot = plotWaves 
    plotGif fn wcols = plotWavesGif wcols fn

instance Plottable [(Double,Double)] where
    plot = plotPnts
    plotGif = flip plotPntsGif

instance Plottable (UVecWave Double, [Double]) where
    plot (w, evts) = plotWaveAndEvents w evts
    plotGif fn (w,es) = plotWaveAndEventsGif w es fn


plotBoth fn a  = plot a >> plotGif fn a


wave2Map w = zip (map (p2t w) [0..(npnts w -1)]) (pntsAsList w)

getLimitsFromGnuplot :: (Show a, Show b) => [(a,b)] -> IO [Double]
getLimitsFromGnuplot pts = do
  nm <- writePoints pts
  gnuplotCmds [plotFilePts nm "points",
               "pause -1",
               "system sprintf(\"echo %g %g %g %g>thezoom\", GPVAL_X_MIN, GPVAL_X_MAX, GPVAL_Y_MIN,GPVAL_Y_MAX)"
              ] False
  fl <- readFile "thezoom"
--  putStr fl
  let nums =  map (filter (/='\n')) . splitOn (==' ') $ fl
  return $ map read nums
  
  
plotWaveAndEvents ::  UVecWave Double -> [Double] -> IO ()
plotWaveAndEvents w tms = do
  fw <- writePoints (wave2Map w)
  fpts <- writePoints $ map (`pair` (maxval w)) tms
  let t1 = mint w
  gnuplotCmds [setRange "x" (mint w) (maxt w), 
               setRange "y" (minval w) (maxval w), 
               noKey,
               inMultiplot [ 
                plotFilePts fw "lines", 
                plotFilePts fpts "points lt rgb \"black\""]
               ] True

plotWaveAndEventsGif ::  UVecWave Double -> [Double] -> String -> IO ()
plotWaveAndEventsGif w tms fn = do
  fw <- writePoints (wave2Map w)
  fpts <- writePoints $ map (`pair` (maxval w)) tms
  let t1 = mint w
  gnuplotCmds [outToFile fn,
               setRange "x" (mint w) (maxt w), 
               setRange "y" (minval w) (maxval w),
               noTopRightAxis,
               noKey,
               inMultiplot [ 
                plotFilePts fw "lines", 
                plotFilePts fpts "points lt rgb \"black\""]
               ] True

noKey = "set nokey"
inMultiplot strs =  "set multiplot\n" ++ unlines strs ++ "unset multiplot"
setRange r v1 v2 = "set "++r++"range ["++(show v1)++":"++ (show v2)++"]"
outToFile fnm = "set terminal gif size 1200,400; set output '"++fnm++"';"
plotFilePts fnm ptype = "plot \""++fnm++"\" with "++ ptype++" "
noTopRightAxis = "set border 3; set xtics nomirror; set ytics nomirror"

data TwoDLims = TwoDL { limx1, limx2, limy1, limy2 :: !Double }

--data a :*: b = !a :*: !b

includeWaveInLims w (TwoDL x1 x2 y1 y2) = TwoDL (min x1 $ mint w)
                                          (max x2 $ maxt w)
                                          (min y1 $ minval w)
                                          (max y2 $ maxval w)

limsOfWave w = TwoDL (mint w)
               (maxt w)
               (minval w)
               (maxval w)

plotWavesFlex :: [(UVecWave Double, Col)] -> String -> IO ()
plotWavesFlex [] _ = print $ "plotWaves: nothing to plot"
plotWavesFlex wscols outCmd = do
  --let ws =  map fst $ wscols
  {-let y1 = minval w1
  let y2 = maxval w1
  let x1 = mint w1
  let x2 = maxt w1-}
  {-let y1 = foldl1 (min) $ map minval ws
  let y2 = foldl1 (max) $ map maxval ws
  let x1 = foldl1 (min) $ map mint ws
  let x2 = foldl1 (max) $ map maxt ws
-}
  (plotCmds:*: TwoDL x1 x2 y1 y2) <- foldM iteratee ([]:*:limsOfWave w1) wscols
  gnuplotCmds ([outCmd, 
                setRange "y" y1 y2,setRange "x" x1 x2, noKey,
                inMultiplot $ plotCmds]) True

    where w1 = fst $ head wscols 
          iteratee (cmds:*: lims) (w,col) = do nm <- writePoints (wave2Map w) 
                                               let cmdnew =plotFilePts nm ("lines "++showGnuPlotCol col) 
                                               return $! (cmdnew:cmds :*: includeWaveInLims w lims)
--foldM


--plotWaves :: (Show vt, DiscreteWave wt vt) => [wt vt] -> IO ()
plotWaves ws = plotWavesFlex ws ""
plotWavesGif ws fn = plotWavesFlex ws (outToFile fn)

plotRegW :: (Show vt, DiscreteWave wt vt) => wt vt -> IO ()
plotRegW w = gnuplot (wave2Map w) "" "lines" True

plotRegWgif :: (Show vt, DiscreteWave wt vt) => wt vt -> String -> IO ()
plotRegWgif w fnm = do
  gnuplot (wave2Map w) (outToFile fnm) "lines" False

plotPntsGif :: (Show a, Show b) => [(a,b)] -> String -> IO ()
plotPntsGif pts fnm 
    = gnuplot pts (outToFile fnm) "points" False

plotPnts pts = gnuplot pts "" "points" True

pair x y = (x, y)


gnuplot :: (Show a, Show b) => [(a,b)] -> String -> String -> Bool -> IO ()
gnuplot pts extra ptype persist
    = do    nm <- writePoints pts
            gnuplotCmds [extra, plotFilePts nm ptype] persist


writePoints :: (Show a, Show b) => [(a,b)] -> IO String
writePoints pts = do
  let contStrs = map (\(x,y)->(show x)++"\t"++(show y)) pts
  nm <- return . (\n->"/tmp/w"++n++".dat") . show . hashUnique =<< newUnique
  writeFile nm $ unlines contStrs
  return nm

gnuplotCmds :: [String] -> Bool -> IO()
gnuplotCmds cmds persist = do
  let persStr = if persist then "-persist" else ""
  let cmdstr = intercalate "\n" cmds
  writeFile "gnuplotCmds" cmdstr
  let execStr ="gnuplot "++persStr++" gnuplotCmds"
  putStrLn cmdstr
  putStrLn execStr
  system  execStr
  return ()


splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn f l@(x:xs)
  | f x = splitOn f xs
  | otherwise = let (h,t) = break f l in h:(splitOn f t)
