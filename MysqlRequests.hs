module MysqlRequests (timeslot, faculty, students, unavail)
where

import Control.Monad
import Database.HDBC
import Database.HDBC.SqlValue as S
import Database.HDBC.MySQL as M

-- mysql requests

sql_timeslot = "SELECT slot_id FROM timeslot ORDER BY date, timeslot.from"
sql_faculty  = "SELECT faculty_id, name, building FROM faculty"
sql_students = "SELECT embark_id, name FROM student"
sql_unavail  = "SELECT timeslot, faculty indepth FROM matrix WHERE student LIKE \"X%\""

-- extraction from database

conn :: MySQLConnectInfo
conn = defaultMySQLConnectInfo {
                       mysqlHost     = "localhost",
                       mysqlPort     = 3306,
                       mysqlUser     = "jie",
                       mysqlPassword = "jie",
                       mysqlDatabase = "matrixJun16",
                       mysqlUnixSocket = "/tmp/mysql.sock"
                    }

timeslot :: IO [(Int, Int)] -- index and database slot ID
timeslot = do conn <- connectMySQL conn
              sql <- quickQuery' conn sql_timeslot []
              return $ extract sql
--      where extract = flip zip [0..] . map (fromSql . head ) Real chronological order
      where extract = flip zip [0..] . switch4 . map (fromSql . head ) -- First day (4 sessions) pushed at the back
            switch4 x = let (a,b) = splitAt 4 x in b ++ a

faculty :: IO [(Int, String, String)] -- prof ID, name, lab
faculty = do conn <- connectMySQL conn
             sql <- quickQuery' conn sql_faculty []
             return $ extract sql
      where extract = map (\[a,b,c] -> (fromSql a, fromSql b,fromSql c) )

students :: IO [(Int, String)] -- stu ID, name
students = do conn <- connectMySQL conn
              sql <- quickQuery' conn sql_students []
              return $ extract sql
      where extract = map (\[a,b] -> (fromSql a, fromSql b) )

unavail :: IO [(Int, Int)] -- slot ID, prof ID
unavail = do conn <- connectMySQL conn
             sql <- quickQuery' conn sql_unavail []
             return $ extract sql
      where extract =  map (\[a,b] -> (fromSql a, fromSql b) )
