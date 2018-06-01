import qualified Data.Map as M (fromList, adjust, lookup, keys, member)
import Data.Map (Map, (!))
import Data.List (findIndices, (\\), sortBy, sort, find, intercalate, sortOn)
import Data.List.Split (splitOn)
import Data.Char (toUpper)
import Data.Function (on)
import Data.Tuple (swap)
import System.IO
import MysqlRequests

-------------------- NEW DATA TYPES --------------------

-- Professors: name, ID, lab, index of busy timeslots
data Prof = Prof {pName::String,
                  pID::String,
                  lab::String,
                  pBusy::[Int]
                  } deriving (Show, Eq)

-- Students: name, ID, list of prof pID for interview , In-Depth interview, index of busy timeslots
data Stu = Stu {sName::String,
                sID::String,
                sIDInterview::[String],
                sInterview::[String],
                sBusy::[Int]
                } deriving (Show, Eq)

-------------------- NEW TYPES SYNONYMS --------------------

-- Interview Schedule: time slot, prof ID, student ID, in-depth
type Schedule =  [(Int, String, String, String)]
type RawProf = [(String, String, String)] -- prof ID, name, lab
type RawStu = [(String, String)] -- stu ID, name
type RawTimseslot = [(Int, Int)] -- DB slot index, chronological index
type RawUnavail = [(Int, String)] -- DB slot index, prof ID
type ProfList = Map String Prof
type StufList = [Stu]

-------------------- FUNCTIONS --------------------

-- Transforms input from Excel/CSV matrix file into Strings
makeMatrix :: String -> RawProf -> RawStu ->  Map String [[String]]  -- Input: csv, output: list of (Stu name, [Prof !name])
makeMatrix csv prof stu = M.fromList $ matrixToIndices prof stu (strToMat csv)
  where strToMat = map listToMat . map (splitOn ",") . tail . (splitOn newLine)
        listToMat (sID:xs) = (sID
                             , map (profIndex!) $ findIndices (== "1") xs -- Interview
                             , map (profIndex!) $ findIndices (== "2") xs -- In-Depth interview
                             )
        profIndex =  M.fromList $ zip [0..] $ tail $  splitOn "," $ head $ (splitOn newLine) csv

-- Intermediate function for makeMatrix: looks up IDs from names (things go wrong here!)
matrixToIndices :: RawProf -> RawStu -> [(String, [String], [String])] -> [(String, [[String]])]
matrixToIndices p s = map toStudent
  where toStudent (st,ints,ids) = (getSt st, [map getPr ints, map getPr ids])
        getSt st = maybe "-1" (const st) $ find (\(x,_) -> format x == format st) s
        getPr pr = maybe "-1" (const pr) $ find (\(p,_,_) -> format p == format pr) p
        format = concat . words

-- Tests which names do not match between the Excel file and the BD
unmatchedNames :: String -> RawProf -> RawStu -> [String]
unmatchedNames csv prof stu = (filter noPr profNames) ++ (filter noSt stuNames)
 where profNames = tail $ splitOn "," $ head $ (splitOn newLine) csv
       stuNames = map (takeWhile (/=',')) $ tail $ (splitOn newLine) csv
       noSt st = (find (\(x,_) -> format x == format st) stu) == Nothing
       noPr pr = (find (\(pid,_,_) -> show pid == format pr) prof) == Nothing
       format = concat . words . map toUpper

-- switches the DB index to the chronological index in the list of anavailabilities
unavailChron :: Map Int Int -> RawUnavail -> RawUnavail
unavailChron ts = map (\(i, p) -> (ts!i , p))

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
makeStu :: Map String [[String]] -> ProfList -> RawStu  -> StufList
makeStu m p stu = map listToStu stu
  where listToStu (sid,name) =
             Stu { sName = name,
                   sID = sid,
                   sIDInterview = sortOn (lab . (p!)) (last $ m!sid), -- ordering by lab to try to bunch interviews
                   sInterview = sortOn (lab . (p!)) (head $ m!sid),
                   sBusy = []}

-- Finds a suitable schedule from inversed timeslots [(chron indec, DB index)], Prof and Stu
findSchedule :: Map Int Int -> ProfList -> StufList -> Schedule
findSchedule ts p ((Stu name sid intID (int:ints) busy): ss)
  | null poss            = []                                 -- No possibilities, fail the branch
  | null ss && null ints = [solution]                         -- last student, last interview
  | length intID==1      = solution: findSchedule ts newp (ss++[news]) -- last in-depth interview, go to next student and put normal interviews at the back
  | null ints            = solution: findSchedule ts newp ss  -- last interview, go to next student
  | not $ null next      = solution: next                     -- possible solution, go with that
  | otherwise            = findSchedule ts p (busys:ss)       -- try again at a different time
    where i = if null intID then int else head intID          -- picks in-depth interview first if there is one
          poss = (M.keys ts) \\ (busy ++ pBusy (p!i))     -- possible times (all timeslots minus prof and student busy times)
          isIDInt = if null intID then "" else "ID"           -- adding "ID" if it is in-depth interview
          solution = (ts!(head poss), pID (p!i), sid, isIDInt) -- element of schedule (timeslot, prod ID, stu ID,in depth?)
          next = findSchedule ts newp (news:ss)               -- Rest of the schedule if the first possibility is chosen
          newp = M.adjust (\l -> l{pBusy=(head poss):pBusy l } ) i p -- adding busy time for prof
          news = if null intID
                 then Stu name sid intID ints ((head poss):busy) -- Adding busy time and prepping for next interview
                 else Stu name sid (tail intID) (int:ints) ((head poss):busy) -- for either in-depth or normal
          busys = Stu name sid intID (int:ints) ((head poss):busy) -- Adding busy time and trying again same interview

-- Transforms the schedule into mysql readable csv
formatCSV :: Schedule -> String
formatCSV s = unlines $ line1 : (map breakSchedule s)
  where breakSchedule = \(ts,pid,sid,depth)
             -> intercalate "\",\"" ["\"0",show ts, sid, pid,depth,"","\""]
        line1 = "\"id\",\"timeslot\",\"student\",\"faculty\",\"indepth\",\"comment\",\"ts\""

newLine :: String
newLine = "\r\n"

-------------------- CHECKS AND MAIN --------------------

-- Prints all values from SQL query
checkDB :: IO ()
checkDB = do
  print <$> timeslot
  print <$> faculty
  print <$> students
  print <$> unavail
  return ()

-- Checks for unmatched names between Excel/CSV file and DB
checkIDs :: IO ()
checkIDs = do
  f <- faculty
  s <- students
  m <- readFile inputFile
  let unmatched = unmatchedNames m f s
  if null unmatched
    then putStrLn "All IDs were matched."
    else putStrLn "Could not find a match in the DB for the following IDs:"
  mapM_ print unmatched

inputFile  = "Intput_Output/IM_Jun18.csv"
outPutFile = "Intput_Output/schedule_Jun18.csv"

-- Build the schedule
main :: IO ()
main = do
  t <- timeslot
  f <- faculty
  s <- students
  u <- unavail
  m <- readFile inputFile
  let matrix = makeMatrix m f s
      uc = unavailChron (M.fromList t) u
      prof = makeProf uc f
      stu = makeStu matrix prof s
      schedule = findSchedule (M.fromList $ map swap t) prof stu
  writeFile outPutFile (formatCSV schedule)
