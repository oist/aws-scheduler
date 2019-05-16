import qualified Data.Map as M
import Data.Map (Map, (!))
import Data.List (findIndices, (\\), sortBy, sort, find, intercalate, sortOn, group, lookup)
import Data.List.Split (splitOn)
import Data.Char (toUpper)
import Data.Function (on)
import Data.Tuple (swap)
import System.IO
import MysqlRequests
import System.Random

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
                sInterview::[String],
                sBusy::[Int],
                set::[Int]
                } deriving (Show, Eq)

-------------------- NEW TYPES SYNONYMS --------------------

-- Interview Schedule: time slot, prof ID, student ID, in-depth
type Schedule =  [(Int, String, String)]
type RawProf = [(String, String, String)] -- prof ID, name, lab
type RawStu = [(String, String)] -- stu ID, name
type RawTimseslot = [(Int, Int)] -- DB slot index, chronological index
type RawUnavail = [(Int, String)] -- DB slot index, prof ID
type ProfList = Map String Prof
type StuList = [Stu]

-------------------- FUNCTIONS --------------------

-- Transforms input from Excel/CSV matrix file into Strings
makeMatrix :: String -> RawProf -> RawStu ->  Map String [String]  -- Input: csv, output: list of (Stu name, [Prof !name])
makeMatrix csv prof stu = M.fromList $ matrixToIndices prof stu (strToMat csv)
  where strToMat = map listToMat . map (splitOn ",") . tail . (splitOn newLine)
        listToMat (sID:xs) = (sID , map (profIndex!) $ findIndices (== "1") xs)
        profIndex =  M.fromList $ zip [0..] $ tail $  splitOn "," $ head $ (splitOn newLine) csv

-- Intermediate function for makeMatrix: looks up IDs from names (things go wrong here!)
matrixToIndices :: RawProf -> RawStu -> [(String, [String])] -> [(String, [String])]
matrixToIndices p s = map toStudent
  where toStudent (st,ints) = (getSt st, map getPr ints)
        getSt st = maybe "-1" (const st) $ find (\(x,_) -> format x == format st) s
        getPr pr = maybe "-1" (const pr) $ find (\(p,_,_) -> format p == format pr) p
        format = concat . words

-- Tests which names do not match between the Excel file and the BD
unmatchedNames :: String -> RawProf -> RawStu -> [String]
unmatchedNames csv prof stu = (filter noPr profNames) ++ (filter noSt stuNames)
 where profNames = tail $ splitOn "," $ head $ (splitOn newLine) csv
       stuNames = map (takeWhile (/=',')) $ tail $ (splitOn newLine) csv
       noSt st = (find (\(x,_) -> format x == format st) stu) == Nothing
       noPr pr = (find (\(pid,_,_) -> pid == format pr) prof) == Nothing
       format = concat . words . map toUpper

-- switches the DB index to the chronological index in the list of unavailabilities
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
makeStu :: Map String [String] -> ProfList -> RawStu  -> StuList
makeStu m p stu = map listToStu stu
  where listToStu (sid,name) =
             Stu { sName = name,
                   sID = sid,
                   sInterview = shuffle sid (m!sid),
                   sBusy = [],
                   set = []
                 }
        shuffle str lst = fst $ fisherYates lst (mkStdGen $ hashString str)

hashString :: String -> Int
hashString = foldr1 (*) . map fromEnum

fisherYates :: RandomGen g => [a] -> g -> ([a], g)
fisherYates a gen = shuffle 1 gen m
  where m = M.fromList $ zip [1..] a
        shuffle i g k
          | i == M.size m = (M.elems k, g)
          | otherwise     = let (j, g') = randomR (i, M.size m) g
                                k' = M.insert i (k!j) $ M.insert j (k!i) k
                            in shuffle (i+1) g' k'

findSchedule :: Map Int Int -> ProfList -> StuList -> Schedule
findSchedule ts p ((Stu name sid (int:ints) busy set): ss)
  | null poss            = []                                 -- No possibilities, fail the branch
  | null ss && null ints = [solution]                         -- last student, last interview
  | null ints && null nextStu = []                            -- Next students has no possibility, fail the branch
  | null ints            = solution: nextStu                  -- last interview, go to next student
  | not $ null next      = solution: next                     -- possible solution, go with that
  | otherwise            = findSchedule ts p (busys:ss)       -- try again at a different time
    where poss = (M.keys ts) \\ (busy ++ pBusy (p!int))       -- possible times (all timeslots minus prof and student busy times)
          poss1 = head poss
          solution = (ts!poss1, pID (p!int), sid)             -- element of schedule (timeslot, prof ID, stu ID,in depth?)
          nextStu = findSchedule ts newp ss
          next = findSchedule ts newp (ss++[news])            -- Rest of the schedule if the first possibility is chosen
          newp = M.adjust (\l -> l{pBusy = poss1: pBusy l } ) int p -- adding busy time for prof
          news = Stu name sid ints (poss1:set) (poss1:set)    -- Adding busy time and prepping for next interview
          busys = Stu name sid (int:ints) (poss1:busy) set    -- Adding busy time and trying again same interview


-- Transforms the schedule into mysql readable csv
formatCSV :: Schedule -> String
formatCSV s = unlines $ line1 : (map breakSchedule s)
  where breakSchedule = \(ts,pid,sid)
             -> intercalate "\",\"" ["\"0",show ts, sid, pid,"","","\""]
        line1 = "\"id\",\"timeslot\",\"student\",\"faculty\",\"indepth\",\"comment\",\"ts\""

newLine :: String
newLine = "\n"

-------------------- CHECKS AND MAIN --------------------

-- Prints all values from SQL query
checkDB :: IO ()
checkDB = do
  timeslot >>= print
  faculty >>= print
  students >>= print
  unavail >>= print

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

-- Checks the number of free spots compared to number of interviews
checkAvail :: IO ()
checkAvail = do
  u <- unavail
  csv <- readFile inputFile
  let count = filter ((>0) . snd) . map (\g -> (head g, 12 - length g)) .
              group . sort . map snd $ u
      r@(ids: _) = map (tail . splitOn ",") $ splitOn newLine csv
      replace = filter (not . null) . zipWith (\i c -> if null c then c else i) ids
      count' = map (\g -> (head g, length g - 1)) $ group $ sort $ concatMap replace r
      check (i, c) = do
        c' <- lookup i count
        if c <= c' then Nothing else Just (i, c, c')
  if all ((==Nothing) . check) count'
    then putStrLn "Availabilities match"
    else do
      putStrLn "Errors: \nFrom database:"
      print count
      putStrLn "From CSV:"
      print count'
      putStrLn "Mismatch:"
      print $ filter (/= Nothing) $ map check count'




inputFile  = "Input_Output/IM_Jun19.csv"
outPutFile = "Input_Output/schedule_Jun19.csv"

-- Build the schedule
makeSchedule :: IO Schedule
makeSchedule = do
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
  return schedule

main :: IO ()
main = makeSchedule >>= update
