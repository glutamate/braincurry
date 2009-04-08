{-# LANGUAGE GADTs, FlexibleInstances, DeriveDataTypeable, UndecidableInstances, OverlappingInstances, NoMonomorphismRestriction#-}

module Query2 where

import NewResult
import ApparatusPersistence
import Database.HDBC.PostgreSQL 
import Database.HDBC 
import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Text.Printf
import SaveWaves
import Waves4
import ArrayWave

import Data.Typeable
import SqlQuery

type Name = String

data TrialInfo = TrialInfo { trialId :: Int, triggerTime:: String, 
                             trialNum :: Int, trialSpec :: String }
                   deriving (Typeable, Show)

data SessionInfo = SessInf { sessNum:: Integer, sessName :: String, 
                             sessStart :: String, sessNtrials:: Integer } 
                   deriving Typeable

instance Show SessionInfo where
    show (SessInf n nm st ntr) = n^++": "++nm++"\t"++st++"\t"++^ntr++" trials\n"

class TrialQuery a
instance Result a => TrialQuery a
instance TrialQuery TrialInfo
instance TrialQuery a => TrialQuery [a]

class QueryResult a where
    fromSqlResult :: [[SqlValue]] -> IO (Maybe a)

instance Result a => QueryResult a where
    fromSqlResult (sqlvls:_) = parseSql sqlvls
    fromSqlResult [] = return Nothing

instance QueryResult a => QueryResult [a] where
    fromSqlResult vls = do mayress <- mapM fromSqlResult $ map (:[]) vls
                           return . Just $ catMaybes mayress

instance QueryResult SessionInfo where
    fromSqlResult ((SqlInteger snum:SqlString sname:SqlString tstart:SqlInteger ntrls:_):_)
        = return . Just $ SessInf snum sname tstart ntrls
    fromSqlResult retSql = do --print retSql
                              return Nothing


instance QueryResult TrialInfo where
    fromSqlResult ((SqlInteger tid:SqlString trigtm:SqlInteger trnm:SqlString spec:_):_)
        = return . Just $ TrialInfo (fromInteger tid) trigtm (fromInteger trnm) spec
    fromSqlResult retSql = do --print retSql
                              return Nothing


data Query r where
    Values :: Result a => Name -> Query [a]
    Trials :: Query [TrialInfo]
    Sessions :: Query [SessionInfo]

    InTrial :: Result a => Int -> Query [a] -> Query a
    InTrials :: TrialQuery a =>  [Int] -> Query [a] -> Query [a]
    InSession :: TrialQuery a => Int -> Query [a] -> Query [a]
    HasResult :: TrialQuery a => Name -> Query [a] -> Query [a]

    Where :: (TrialQuery a, Result b) => Name -> Oper -> b -> Query [a] -> Query [a]
    SpecLike :: String -> Query [a] -> Query [a]

--    Process :: (a->b) -> Query a -> Query b

numVal :: Name -> Query [Double]
numVal = Values

waves :: Name -> Query [UVecWave Double]
waves = Values

spikes :: Query [Spike]
spikes = Values "spike"


--instance Functor Query where
--    fmap f qa = Process f qa

class TableShow a where
    tableShow :: a -> [String]
    colNames :: a -> [String]

data Oper = Eq | Lt | Gt |Like
toSqlOper Eq = "="
toSqlOper Lt = "<"
toSqlOper Gt = ">"
toSqlOper Like = " LIKE "


qType :: Query r -> r
qType = undefined

newtype MIO a = MIO {runMIO :: IO (Maybe a) }

instance Functor MIO where
    fmap f mia = MIO $ fmap (fmap f) (runMIO mia)

instance Monad MIO where
    return x = MIO (return $ Just x) 
    mia >>= f = MIO $ do ma <- runMIO mia
                         case ma of
                           Just a -> runMIO $ f a
                           Nothing -> return Nothing

instance MonadPlus MIO where
    mzero = MIO $ return Nothing
    mplus mia1 mia2 = MIO $ do ma1 <- runMIO mia1
                               case ma1 of
                                Just _ -> return ma1
                                Nothing -> runMIO mia2
                                  
instance MonadIO MIO where
    liftIO ioa = MIO $ fmap Just ioa  

io = liftIO
handleSqlError' mioa = MIO $ handleSqlError (runMIO mioa)

--class MonadAsk m q | m->q where
--    ask :: (QueryResult r) => Query r -> m (q r)

ask :: QueryResult r => Query r -> IO (Maybe r)
ask q = handleSqlError $
	do 	--print sql
		qres <- uncurry (quickQuery' unsafeConn) sql {-  -}
                --print qres
		fromSqlResult qres
	where	sqlparts = queryToSql q
		sql = buildSqlSelect sqlparts

queryToSql :: Query r -> SqlExpr
queryToSql q@(Values nm) = noopSql {flds=map (\f->("rv."++f,[])) flds,
					tbls=[("result as r", []), (tbl++" as rv", [])],
					wh=[("r.name=? and rv.result_id = r.id", [toSql nm])] }
    where tbl = sqlTable . head $ qType q
          flds= fetchSqlFields . head $ qType q
queryToSql (Sessions)  
    = noopSql {flds=[("s.id",[]), ("s.name",[]), ("CAST (s.start_time AS text)",[]),
                     ("(select count(*) from trial as t where t.session_id = s.id) as ntrials",[])],
               tbls=[("session as s", [])]
              }
queryToSql (Trials) = noopSql {flds=[("r.trial_id",[]), 
                                     ("CAST (t.trigger_time AS text)",[]),
                                     ("t.number_in_session",[]),
                                     ("t.spec",[])],
			       tbls=[("result as r, trial as t", [])],
                               wh=[("t.id = r.trial_id",[])],
			       dsctOn=[("(r.trial_id)",[])]}

queryToSql (InTrial tn qr) = q {wh=wh q++[trstrct]}
	where 	q= queryToSql qr
		trstrct = ("r.trial_id=?", [toSql tn])

queryToSql (InSession sn qr) = q {wh=wh q++[srstrct]} 
	where 	q= queryToSql qr
		srstrct = ("(select session_id from trial where id = r.trial_id)=?", [toSql sn])
queryToSql (Where nm op r qr) = q {wh=wh q++[wh']}
	where 	q= queryToSql qr
                tbl = sqlTable r
		wh' = ("(select value from "++tbl++" where trial_id = r.trial_id and name=?)"++(toSqlOper op)++^r, 
				  [toSql nm])
queryToSql (SpecLike spec qr) = q {wh=wh q++[wh']} 
	where 	q= queryToSql qr
                wh' = ("(select spec from trial where id = r.trial_id ) SIMILAR TO E'%"++spec++"(,|\\])%'", 
				  [])
--select 'foo 5' SIMILAR TO 'foo 5(,|\])'
queryToSql (HasResult nm qr) = q {wh=wh q++[wh']}
	where 	q= queryToSql qr
		wh' = ("(select count(*) from results where trial_id = r.trial_id and name=?)>0", 
				  [toSql nm])
queryToSql (InTrials trs qr) = q {wh=wh q++[wh']} 
	where 	q= queryToSql qr
                wh' = ("r.trial_id IN ("++(intercalate ", " $ map show trs)++")", 
				  [])



getAnalysisParameter :: Read a => String -> Int -> IO (Maybe a)
getAnalysisParameter nm trid = do
  qres <- uncurry (quickQuery' unsafeConn) (buildSqlSelect sql) {-  -}
  case qres of
    (SqlString vl:_):_ -> return . Just $ read vl 
    _ -> return Nothing
    where sql = noopSql {flds = [("value", [])],
                         tbls = [("analysis_parameter",[])],
                         wh = [("trial_id_start<=? AND trial_id_stop>=? AND name=?",
                                [toSql trid, toSql trid, toSql nm])], limit = Just 1}

setAnalysisParameter :: Show a => String -> Int -> Int -> a ->IO ()
setAnalysisParameter nm tr1 tr2 vl = do
  run unsafeConn (concat ["insert into analysis_parameter",
                          "(name, value, trial_id_start, trial_id_stop)",
                          "VALUES (?,?,?,?)"]) [toSql nm, toSql $ show vl, toSql tr1,toSql tr2]
  return ()