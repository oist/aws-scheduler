module MysqlRequests (timeslot, faculty, students, unavail, update)
where

import Control.Monad
import Database.HDBC
--import Database.HDBC.SqlValue as S
import Database.HDBC.MySQL as M

-- mysql requests

sql_timeslot = "SELECT slot_id FROM timeslot ORDER BY date, timeslot.from"
sql_faculty  = "SELECT faculty_id, name, building FROM faculty"
sql_students = "SELECT embark_id, name FROM student"
sql_unavail  = "SELECT timeslot, faculty FROM matrix WHERE student LIKE \"X%\""

-- extraction from database

-- Get connection info (username, password...) from config file
getConnectionInfo :: IO MySQLConnectInfo
getConnectionInfo = do
  txt <- readFile "Mysql.config"
  let [h, u, p, d] = map (last . words) $ lines txt
  return defaultMySQLConnectInfo
           { mysqlHost = h
           , mysqlUser = u
           , mysqlPassword = p
           , mysqlDatabase = d
           }

timeslot' :: IO [(Int, Int)] -- index and database slot ID
timeslot' = do info <- getConnectionInfo
               conn <- connectMySQL info
               extract <$> quickQuery' conn sql_timeslot []
      where extract = flip zip [0..] . switch4 . map (fromSql . head ) -- First day (4 sessions) pushed at the back
            switch4 x = let (a,b) = splitAt 4 x in b ++ a

timeslot :: IO [(Int, Int)] -- (cheating) index and database slot ID
timeslot = return $ zip order [0..]
        -- order is mixed: Tuesday first, first AM/PM sessions first...
  where order = [5, 9, 6, 10, 7, 11, 8, 12, 13, 14, 15, 22] ++ [16..21] ++ [1..4]

faculty :: IO [(String, String, String)] -- prof ID, name, lab
faculty = do info <- getConnectionInfo
             conn <- connectMySQL info
             map extract <$> quickQuery' conn sql_faculty []
      where extract :: [SqlValue] -> (String, String, String)
            extract [a,b,c] = (fromSql a, fromSql b,fromSql c)

students :: IO [(String, String)] -- stu ID, name
students = do info <- getConnectionInfo
              conn <- connectMySQL info
              map extract <$> quickQuery' conn sql_students []
      where extract :: [SqlValue] -> (String, String)
            extract [a,b] = (fromSql a, fromSql b)

unavail :: IO [(Int, String)] -- slot ID, prof ID
unavail = do info <- getConnectionInfo
             conn <- connectMySQL info
             map extract <$> quickQuery' conn sql_unavail []
      where extract :: [SqlValue] -> (Int, String)
            extract [a,b] = (fromSql a, fromSql b)

update :: [(Int, String, String, String)] -> IO ()
update updt = do
  info <- getConnectionInfo
  conn <- connectMySQL info
  stmt <-
    prepare
     conn
     "INSERT INTO matrix set student = ?, indepth = ?, timeslot = ?, faculty = ?;"
  executeMany stmt $
      map (\(ts, fac, st, depth) -> [toSql st, toSql depth, toSql ts, toSql fac]) updt
  commit conn
  disconnect conn
