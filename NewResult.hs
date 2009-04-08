{-# LANGUAGE ExistentialQuantification, FlexibleInstances, DeriveDataTypeable #-}

module NewResult where

import Data.Typeable
import Data.Char
import Data.Maybe
import Data.List
import Database.HDBC.PostgreSQL 
import Database.HDBC 
import SaveWaves
import Waves4

import ArrayWave


class (Typeable r, Show r, Eq r) => Result r where
    store :: Connection -> Integer -> String -> r -> IO ()
    showShort :: r-> String
    showShort = show
    parseSql :: [SqlValue] -> IO (Maybe r)
    sqlTable ::r -> String
    fetchSqlFields :: r -> [String]
    fetchSqlFields _ = ["value"]
    fetchSqlOrder :: r-> String
    fetchSqlOrder _ = ""

--instance Result String 
--instance Result Bool
--instance Result Int



storeResult :: Connection -> Integer -> String -> Integer -> IO Integer
storeResult conn tid nm tp 
    = do sqid <- quickQuery' conn ("INSERT INTO result (name, trial_id, result_type) VALUES (?, ?, ?) returning id")
                       [toSql nm, toSql tid, toSql tp]
         case head . head $ sqid of 
           SqlInteger i -> return i
           SqlInt32 i -> return . toInteger $ i
           SqlInt64 i -> return . toInteger $ i

storeSimple :: (SqlType r) => String -> Integer -> Connection -> Integer -> String -> r -> IO ()
storeSimple tbl tp conn tid nm val  
        = do res_id <- storeResult conn tid nm tp
             run conn ("INSERT INTO "++tbl++"(result_id, value) VALUES (?, ?)") 
                       [toSql res_id, toSql val] 
             
             return ()

instance Result Double where
    store = storeSimple "result_num" 1
    parseSql (SqlDouble x:_) = return $ Just x
    sqlTable _ = "result_num"
        

instance Result Integer where
    store = storeSimple "result_num" 1
    sqlTable _ = "result_num"
    parseSql (sv:_) = case sv of 
                        SqlInteger i -> return $ Just i
                        SqlInt32 i -> return . Just . toInteger $ i
                        SqlInt64 i -> return . Just . toInteger $ i

instance Result Int where
    store = storeSimple "result_num" 1
    sqlTable _ = "result_num"
    parseSql (sv:_) = case sv of 
                        SqlInteger i -> return . Just $ fromInteger i
                        SqlInt32 i -> return . Just . fromInteger . toInteger $ i
                        SqlInt64 i -> return . Just . fromInteger . toInteger $ i

instance Result [Double] where
    sqlTable _ = "result_list_num"
    fetchSqlOrder _ = "pntix"
    store conn tid nm vals  
        = do res_id <-storeResult conn tid nm 2
             storeListNum vals (0::Integer) res_id >>  return ()
              where storeListNum (v:vs) n resid
                        = run conn "INSERT INTO result_list_num(result_id, value, pntix) VALUES (?, ?, ?)" 
                                            [toSql resid, toSql v, toSql n] >> storeListNum vs (n+1) resid
                    storeListNum [] n _ = return ()


instance Result Spike where
    sqlTable _ = "result_spike"
    store conn tid nm (Spike a npr w tpd cn tsp wf)
          = do rid <- storeResult conn tid nm 4
               run conn "INSERT INTO result_spike(result_id, amplitude, \"negPosRatio\", width, \"tPeakDiff\", \"clusterNum\", tspike) VALUES (?, ?, ?, ?, ?, ?, ?) returning id" 
                                            [toSql rid, toSql a, toSql npr, toSql w, toSql tpd, toSql cn, toSql tsp] >> return ()
    fetchSqlFields _ = ["amplitude","negPosRatio","width","tPeakDiff","clusterNum","tspike"]
    parseSql (SqlDouble a:SqlDouble npr:SqlDouble w:SqlDouble tpd:SqlInteger cn:SqlDouble tsp:_)
        = return $ Just (Spike a npr w tpd (Just cn) tsp Nothing)
    parseSql (SqlDouble a:SqlDouble npr:SqlDouble w:SqlDouble tpd:SqlNull:SqlDouble tsp:_)
        = return $ Just (Spike a npr w tpd Nothing tsp Nothing)
    parseSql s = do print s
                    putStrLn "not a spike"
                    return Nothing

wavesDir = "/home/tomn/waves/"

{-instance Result (RegularWave Double) where
    sqlTable _ = "result_wave"
    store conn tid nm w@(RegularWave pts sdt tleft npts)  
        = do let fn = wavesDir++(show tid)++"_"++nm++".twv"
             saveWave fn w
             rid <- storeResult conn tid nm 3
             run conn "INSERT INTO result_wave(result_id, dt, tleft, numpnts, datapath) VALUES (?, ?, ?, ?, ?) returning id" 
                                            [toSql rid, toSql sdt, toSql tleft, toSql npts, toSql fn] >> return ()
    showShort = shownpnts 5 
    fetchSqlFields _ = ["datapath"]
    parseSql sqres = gtWv sqres
		where 	gtWv (SqlString wpath:_) 
				= return . Just =<< loadWave (wpath)
			gtWv _ = return Nothing
-}
instance Result (UVecWave Double) where
    sqlTable _ = "result_wave"
    store conn tid nm w@(UVecWave pts sdt tleft npts)
        = do let fn = wavesDir++(show tid)++"_"++nm++".twv"
             saveWave fn w
             rid <- storeResult conn tid nm 3
             run conn "INSERT INTO result_wave(result_id, dt, tleft, numpnts, datapath) VALUES (?, ?, ?, ?, ?) returning id" 
                                            [toSql rid, toSql sdt, toSql tleft, toSql npts, toSql fn] >> return ()
    showShort w = "arraywave"++(show $uWvNfirst 5 w)
    fetchSqlFields _ = ["datapath"]
    parseSql sqres = gtWv sqres
		where 	gtWv (SqlString wpath:_) 
				= return . Just =<< loadWave (wpath)
			gtWv _ = return Nothing


data AnyResult = forall r. Result r => AnyRes r 

instance Eq AnyResult where
    (==) = eqAnyRes

eqAnyRes :: AnyResult -> AnyResult -> Bool
eqAnyRes (AnyRes r1) (AnyRes r2) = case cast r1 `asTypeOf` (Just r2) of
                                     Just r1' -> r1' == r2
                                     Nothing -> False
    

instance Show AnyResult where
    show (AnyRes r) = show r
{-
data Spec = Analyse [String] ([AnyResult]->Maybe AnyResult) String

analyse1 :: (Result a, Result b) => String->(a->b)->String->Spec
analyse1 src f targ = Analyse [src] (\[AnyRes inres]->case cast inres `asTypeOf` undefA of
                                                        Just theA -> Just . AnyRes $ f theA 
                                                        Nothing -> Nothing) targ
                      where undefB = f undefA
                            undefA = undefined
-}
incrementAnal ::  [(String, AnyResult)] --existing results
              -> [([String], [AnyResult]->Maybe AnyResult, String)] --analyses to perform
              -> [(String, AnyResult)] -- new results
incrementAnal ress []    = ress
incrementAnal ress anals = case null todo of
                             True -> ress -- done, can't do rest
                             False -> incrementAnal (ress++(catMaybes $ map doAnal todo)) postpone
    where (todo, postpone)      = partition canPerform anals  
          canPerform (nms, _, _) =  and $ map (`elem` map fst ress) nms
          doAnal (nreqs, lam, nres) = case lam $ map (\nm->fromJust (lookup nm ress)) nreqs of
                                          Just anyres -> Just (nres, anyres)
                                          Nothing -> Nothing

{-
myspec = [analyse1 "theString" (map toUpper) "upperString",
          analyse1 "theNum" (+(1::Int)) "numplus1",
          analyse1 "foo" (+(2::Int)) "numplus2"]

stripSpec (Analyse src f targ) = (src, f, targ)

myRess = [("theString", AnyRes "hello world"), ("theNum", AnyRes (5::Int))]

-}

data Spike = Spike {
      amp :: !Double,
      negPosRatio :: !Double,
      width :: !Double,
      tPeakDiff :: !Double,
      clusterNum :: !(Maybe Integer),
      tspike :: !Double,
      waveForm :: Maybe (UVecWave Double)
} deriving (Show,Read, Eq, Typeable)

