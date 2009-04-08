{-# LANGUAGE NoMonomorphismRestriction #-}

module ApparatusPersistence where

-- import GNUPlot
import Data.Maybe
import Data.List
import Waves4
import ArrayWave
import System.IO
import System.Time
import System.IO.Unsafe
import System.Process

import SaveWaves
import Control.Concurrent.STM
import Database.HDBC.PostgreSQL 
import Database.HDBC 
--import LocustTypes
import Apparatus hiding (run)
import NewResult
import Data.Typeable

-- results continuation

--type TRes = (TrialDetails,[(String, Result)])
--type Reporter = TRes -> IO TRes

--discard :: TRes -> IO ()
--discard _ = return ()

--showLocally :: Reporter
showLocally td ress = runCommand "killall gnuplot 2>/dev/null" >> mapM_ showLocally' ress 

showLocally' (nm, AnyRes res)
	= putStrLn (nm++": "++(showShort res)) >> possiblyPlot res

possiblyPlot w = case cast w `asTypeOf` (Just w1) of
                                     Just w' -> plotRegW w'
                                     Nothing -> return ()

plotToGif (TD tspec timeStart sessn trn) ress = mapM_ plotToGif' ress
    where plotToGif' (nm, AnyRes res) = case cast res `asTypeOf` (Just w1) of
                                          Just w' -> plotRegWgif w' $ fname nm
                                          Nothing -> return ()
          fname n = graphsDir++(show sessn)++"_"++(show trn)++"_"++n++".gif"

logToFile td@(TD tspec timeStart sessn trn) ress 
    = appendFile ("log_"++^sessn++".txt") $ logStr td ress 
                  where s ++^ ss = s++(show ss)


logToScreen td ress = putStrLn $ logStr td ress

logStr td@(TD tspec timeStart sessn trn) ress = 
    concat [preamp, "\nTrial Specification:\n", catMapIndent show tspec,"Results:\n", catMapIndent show' ress]
        where preamp = "\nTrial "++^trn++" in session "++^sessn++" started "++^timeStart
              show' (nm, AnyRes res) =nm++": "++(showShort res)
              catMapIndent f = concat . map ((\s->' ':s++"\n") . f)
              s ++^ ss = s++(show ss)


{-storeInFile :: FilePath -> Reporter
storeInFile fl (td,results) = do writeFile fl . show . filter (shouldStore . snd) $ results
                                 return (td,results)

incrementallyStoreInFile :: (Int -> IO FilePath) -> TVar Int -> Reporter
incrementallyStoreInFile fpact ctr (td,ress) 
    = do num <- incCounter ctr
         fp <- fpact num
         storeInFile fp ress
         return (td,ress)
-}
dateString :: IO String
dateString = do calTm <- toCalendarTime =<< getClockTime
                let d = show $ ctDay calTm
                    d3 = padZeros 2 d
                    m = take 3 . show $ ctMonth calTm
                    y = show $ (ctYear calTm - 2000) -- look out for Y3K
                    y2 = padZeros 2 y
                return $ d3++m++y2
    where padZeros n s = reverse . take n $ reverse s ++ zeros
          zeros = '0':zeros

-- relational

connectDB constr = do 	putStrLn $ "Connecting to PostgreSQL..." -- ++connString
                        connectPostgreSQL constr

unsafeConnString = "host=localhost dbname=labdata user=postgres password=foo"
wavesDir = "/home/tomn/waves/"
graphsDir = "/home/tomn/graphs/"
unsafeConn = unsafePerformIO $ connectDB unsafeConnString
listTrials conn = quickQuery' conn 
                "SELECT * FROM trial" []
listSessions conn = handleSqlError $ quickQuery' conn 
               				 "SELECT * FROM session" []
disconnectUnsafe = disconnect unsafeConn

storeInSQL = storeSQL unsafeConn

newSessionFromSQL = newSessionFromSQL' unsafeConn
continueLastSession = continueLastSession' unsafeConn

newSessionFromSQL' :: Connection -> String -> IO ()
newSessionFromSQL' conn nm 
    = handleSqlError $ do   (((SqlInteger tid):_):_) <- quickQuery' conn 
                                      "INSERT INTO session (name) VALUES (?) RETURNING id" [toSql nm]
                            commit conn
                            print ("starting session #"++show tid)
                            setCounter sessionNum tid

continueLastSession' conn 
    = handleSqlError $ do   (((SqlInteger sid):_):_) <- quickQuery' conn 
                                                "SELECT id from session order by id desc limit 1" []
                            setCounter sessionNum sid
                            tidQRes <- quickQuery' conn
                                    "SELECT number_in_session from trial where session_id=? order by number_in_session desc limit 1" [toSql sid]
                            tid <- return $ case tidQRes of
                                                (((SqlInteger tid'):_):_) -> tid'
                                                _ -> 0 
                            setCounter trialNum tid
                            putStrLn $ "Continuing session #"++(show sid)++" after trial #"++(show tid)


storeSQL :: Show o => Connection -> TrialDetails o -> [(String, AnyResult)] -> IO ()
storeSQL dbConn td@(TD tspec timeStart sessn trn) ress
    = handleSqlError $
      do putStrLn $ "sending new trial "++(show trn)++" in session id="++(show sessn)++" to database"
         nm <- return (tspec `nameOr` "") 
         (((SqlInteger tid):_):_) <- quickQuery' dbConn 
                                    "INSERT INTO trial(spec, trigger_time, session_id, number_in_session, name) VALUES (?, to_timestamp(?), ?, ?, ?) returning id" 
                                    [toSql $ show tspec, toSql timeStart, toSql sessn, toSql trn, toSql nm]
         mapM_ (storeSQLRes tid) ress
         commit dbConn
         return ()
    where storeSQLRes tid (nm, AnyRes r) = store dbConn tid nm r

{- 
              = run dbConn "INSERT INTO result_num(name, trial_id, value) VALUES (?, ?, ?)" 
                               [toSql nm, toSql tid, toSql val] >> return ()
          storeSQLRes tid (nm, ResListNum vals) 
              = storeListNum vals (0::Integer) >> return ()
              where storeListNum (v:vs) n 
                        = run dbConn "INSERT INTO result_list_num(name, trial_id, value, pntix) VALUES (?, ?, ?, ?)" 
                                            [toSql nm, toSql tid, toSql v, toSql n] >> storeListNum vs (n+1)
                    storeListNum [] n = return ()
          storeSQLRes tid (nm, ResWave w@(RegularWave pts sdt tleft npts)) 
              = do let fn = wavesDir++(show tid)++"_"++nm++".twv"
                   saveWave fn w
                   run dbConn "INSERT INTO result_wave(name, trial_id, dt, tleft, numpnts, datapath) VALUES (?, ?, ?, ?, ?, ?) returning id" 
                                   [toSql nm, toSql tid, toSql sdt, toSql tleft, toSql npts, toSql fn] >> return ()
          storeSQLRes _ _ = return ()
-}