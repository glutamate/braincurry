module Persistence where

import GNUPlot
import Data.Maybe
import Data.List
import Waves2
import System.IO
import System.Time
import Results
import Trials
import System.IO.Unsafe
import SaveWaves
import Control.Concurrent.STM
import Database.HDBC.PostgreSQL 
import Database.HDBC 

shouldStore (ResGraph _) = False
shouldStore _ = True

readNumList :: FilePath-> IO [Double]
readNumList fp = do 	contents <- readFile fp
			res <- return . map read . lines $ contents
			return res

mkCounter n = newTVarIO n
incCounter ctr = atomically $ do v <- readTVar ctr 
                                 writeTVar ctr (v+1)
                                 return v
setCounter ctr n = atomically $ writeTVar ctr n
readCounter ctr = atomically $ readTVar ctr 
-- results continuation

type TRes = (TrialDetails,[(String, Result)])
type Reporter = TRes -> IO TRes

discard :: TRes -> IO ()
discard _ = return ()

showLocally :: Reporter
showLocally (td,ress) = mapM_ showLocally' ress >> return (td,ress)

showLocally' (nm, ResGraph g)
	=  plotResGraph nm g
showLocally' (nm, ResWave w)
	= putStrLn (nm++": "++(shownpnts 5 w)) 
showLocally' (nm, res)
	= putStrLn (nm++": "++(show res)) 


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

connectDB = do 	putStrLn $ "Connecting to PostgreSQL..." -- ++connString
		connectPostgreSQL connString
connString = "host=localhost dbname=labdata user=postgres password=foo"
wavesDir = "/home/tomn/waves/"
unsafeConn = unsafePerformIO $ connectDB
listTrials = quickQuery' unsafeConn 
                "SELECT * FROM trial" []
listSessions = handleSqlError $ quickQuery' unsafeConn 
               				 "SELECT * FROM session" []
disconnectUnsafe = disconnect unsafeConn

storeSQL :: Connection -> Reporter
storeSQL dbConn (td@(TD tspec timeStart sessn trn),ress)
    = handleSqlError $
      do putStrLn $ "sending new trial "++(show trn)++" in session id="++(show sessn)++" to database"
	 nm <- return (tspec `nameOr` "") 
	 (((SqlInteger tid):_):_) <- quickQuery' dbConn 
                                    "INSERT INTO trial(spec, trigger_time, session_id, number_in_session, name) VALUES (?, to_timestamp(?), ?, ?, ?) returning id" 
                                    [toSql $ show tspec, toSql timeStart, toSql sessn, toSql trn, toSql nm]
         mapM_ (storeSQLRes tid) ress
         commit dbConn
         return (td,ress)
    where storeSQLRes tid (nm, ResNum val) 
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
