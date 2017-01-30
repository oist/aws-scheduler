import qualified Data.Map as M (Map, (!), fromList, update, lookup, keys)
import Data.List (findIndices, (\\), sortBy, sort, find, intercalate)
import Data.List.Split (splitOn)
import Data.Char (toUpper)
import Data.Function (on)
import Data.Tuple (swap)
import System.IO
import MysqlRequests

-------------------- NEW DATA TYPES --------------------

-- Professors: name, ID, lab, index of busy timeslots
data Prof = Prof {pName::String,
                  pID::Int,
                  lab::String,
                  pBusy::[Int]
                  } deriving (Show, Eq)

-- Students: name, ID, list of prof pID for interview , In-Depth interview, index of busy timeslots
data Stu = Stu {sName::String,
                sID::String,
                sIDInterview::[Int],
                sInterview::[Int],
                sBusy::[Int]
                } deriving (Show, Eq)

-------------------- NEW TYPES SYNONYMS --------------------

-- Interview Schedule: time slot, prof ID, student ID, in-depth
type Schedule =  [(Int, Int, String, String)]
type RawProf = [(Int, String, String)] -- prof ID, name, lab
type RawStu = [(String, String)] -- stu ID, name
type RawTimseslot = [(Int, Int)] -- DB slot index, chronological index
type RawUnavail = [(Int, Int)] -- DB slot index, prof ID
type ProfList = M.Map Int Prof
type StufList = [Stu]

-------------------- FUNCTIONS --------------------

-- Transforms input from Excel/CSV matrix file into Strings
makeMatrix :: String -> RawProf -> RawStu ->  M.Map String [[Int]]  -- Input: csv, output: list of (Stu name, [Prof !name])
makeMatrix csv prof stu = M.fromList $ matrixToIndices prof stu (strToMat csv)
  where strToMat s = map listToMat $ map (splitOn ",") $ tail $ (splitOn "\r\n")  s
        listToMat (l:f:xs) = (l++", "++f,
                           [map (profIndex M.!) $ findIndices (== "1") xs , -- Interview
                            map (profIndex M.!) $ findIndices (== "2") xs ]) -- In-Depth interview
        profIndex =  M.fromList $ zip [0..] $ drop 2 $  splitOn "," $ head $ (splitOn "\r\n") csv

-- Intermediate function for makeMatrix: looks up IDs fro names (things go wrong here!)
matrixToIndices :: RawProf -> RawStu -> [(String, [[String]])] -> [(String, [[Int]])]
matrixToIndices _ _ [] = []
matrixToIndices p s ((st,[ints,ids]):ms) = student : matrixToIndices p s ms
  where student = (getSt st, [map getPr ints,map getPr ids] )
        getSt st = case find (\x -> (cap $ snd x) == cap st) s of
                        Just (sid, _) -> sid
                        Nothing -> "-1"
        getPr pr = case find (\(_,name,_) -> cap name == cap pr) p of
                              Just (pid,_,_) -> pid
                              Nothing -> -1
        cap = concat . words . map toUpper

-- Tests which names do not match between the Excel file and the BD
unmatchedNames :: String -> RawProf -> RawStu -> [String]
unmatchedNames csv prof stu = (filter noPr profNames) ++ (filter noSt stuNames)
 where profNames = drop 2 $  splitOn "," $ head $ (splitOn "\r\n") csv
       stuNames = map (intercalate ", " . take 2) $ map (splitOn ",") $ tail $ (splitOn "\r\n") csv
       noSt st = (find (\x -> (cap $ snd x) == cap st) stu) == Nothing
       noPr pr = (find (\(_,name,_) -> cap name == cap pr) prof) == Nothing
       cap = concat . words . map toUpper

-- switches the DB index to the chronological index in the list of anavailabilities
unavailChron :: M.Map Int Int -> RawUnavail -> RawUnavail
unavailChron ts = map (\(i, p) -> (ts M.! i , p))

-- Builds a list of Prof from SQL requests results
makeProf :: RawUnavail -> RawProf -> ProfList
makeProf unav = M.fromList . map (listToProf unav)
  where listToProf unav (pid, name, lab)  =
            (pid,
             Prof { pName = name,
                    pID = pid,
                    lab = lab,
                    pBusy = sort $ map fst $ filter (\(_, b) -> b==pid) unav
                    } )

-- Builds a list of Stu from Excel/CSV matrix info and SQL requests results
makeStu :: M.Map String [[Int]] -> ProfList -> RawStu  -> StufList
makeStu m p stu = map (listToStu m p) $ filter (\(sid,_) -> M.lookup sid m /= Nothing ) stu
  where listToStu m p (sid,name) =
             Stu { sName = name,
                   sID = sid,
                   sIDInterview = sortBy laborder (last $ m M.! sid),
                   sInterview = sortBy laborder (head $ m M.! sid),
                   sBusy = []}
        laborder = compare `on` lab.(p M.!) -- ordering by lab to try to bunch interviews

-- Finds a suitable schedule from inversed timeslots [(chron indec, DB index)], Prof and Stu
findSchedule :: M.Map Int Int -> ProfList -> StufList -> Schedule
findSchedule ts p ((Stu name sid intID (int:ints) busy): ss)
  | null poss            = []                                 -- No possibilities, fail the branch
  | null ss && null ints = [solution]                         -- last student, last interview
  | length intID==1      = solution: findSchedule ts newp (ss++[news]) -- last in-depth interview, go to next student and put normal interviews at the back
  | null ints            = solution: findSchedule ts newp ss  -- last interview, go to next student
  | not $ null next      = solution: next                     -- possible solution, go with that
  | otherwise            = findSchedule ts p (busys:ss)       -- try again at a different time
    where i = if null intID then int else head intID          -- picks in-depth interview first if there is one
          poss = (M.keys ts) \\ (busy ++ pBusy (p M.! i))     -- possible times (all timeslots minus prof and student busy times)
          isIDInt = if null intID then "" else "ID"           -- adding "ID" if it is in-depth interview
          solution = (ts M.! (head poss), pID (p M.! i), sid, isIDInt) -- element of schedule (timeslot, prod ID, stu ID,in depth?)
          next = findSchedule ts newp (news:ss)               -- Rest of the schedule if the first possibility is chosen
          newp = M.update (\l -> Just $ l{pBusy=(head poss):pBusy l } ) i p -- adding busy time for prof
          news = if null intID then Stu name sid intID ints ((head poss):busy) -- Adding busy time and prepping for next interview
                               else Stu name sid (tail intID) (int:ints) ((head poss):busy) -- for either in-depth or normal
          busys = Stu name sid intID (int:ints) ((head poss):busy) -- Adding busy time and trying again same interview

-- Transforms the schedule into mysql reeadable csv
formatCSV :: Schedule -> String
formatCSV s = unlines $ line1 : (map breakSchedule s)
  where breakSchedule = \(ts,pid,sid,depth)
             -> intercalate "\",\"" ["\"0",show ts, sid, show pid,depth,"","\""]
        line1 = "\"id\",\"timeslot\",\"student\",\"faculty\",\"indepth\",\"comment\",\"ts\""

-------------------- CHECKS AND MAIN --------------------

-- Prints all values from SQL query
checkDB :: IO ()
checkDB = do
  t <- timeslot
  f <- faculty
  s <- students
  u <- unavail
  print t
  print f
  print s
  print u

-- Checks for unmatched names between Excel/CSV file and DB
checkNames :: IO ()
checkNames = do
  f <- faculty
  s <- students
  m <- readFile "Intput_Output/IM_Feb17.csv"
  let names = unmatchedNames m f s
  if names /= []
    then putStrLn "Could not find a match in the DB for the following names:"
    else putStrLn "All names were matched."
  mapM_ print names

-- Build the schedule
main :: IO ()
main = do
  t <- timeslot
  f <- faculty
  s <- students
  u <- unavail
  m <- readFile "Intput_Output/IM_Feb17.csv"
  let matrix = makeMatrix m f s
      uc = unavailChron (M.fromList t) u
      prof = makeProf uc f
      stu = makeStu matrix prof s
      schedule = findSchedule (M.fromList $ map swap t) prof stu
  writeFile "Intput_Output/schedule_Feb17.csv" (formatCSV schedule)
