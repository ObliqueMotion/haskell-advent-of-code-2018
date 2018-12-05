module Main where
import Data.List
import Data.List.Split

maxTime = 60
awakes  = (repeat 0)
asleeps = (repeat 1)

data LogType = GuardId | Awake | Asleep deriving (Eq,Show)

parseId :: String -> Int
parseId log = read $ tail ((words log) !! 3) ::Int

parseLog :: String -> (Int,LogType)
parseLog log
    | startsShift log = (parseId   log, GuardId)
    | fallsAsleep log = (parseTime log, Asleep)
    | wakesUp     log = (parseTime log, Awake)
    where startsShift = isInfixOf "begins shift"
          fallsAsleep = isInfixOf "falls asleep"
          wakesUp     = isInfixOf "wakes up"

data Record  = Record Int [Int] deriving (Show)
instance Eq Record where
    (Record id1 _) == (Record id2 _) = id1 == id2
instance Ord Record where
    (Record id1 _) `compare` (Record id2 _) = id1 `compare` id2

newRecord :: Int -> Record
newRecord guardId = Record guardId []

totalTime :: Record -> Int
totalTime (Record guardId timeline) = (length timeline)

appendTime :: Record -> [Int] -> Record
appendTime (Record guardId timeline) logValues = Record guardId (timeline ++ logValues)

complete :: Record -> Record
complete (Record guardId []) = (Record guardId (take maxTime awakes))
complete (Record guardId timeline)
    | (length timeline) == maxTime = (Record guardId timeline)
    | otherwise = (Record guardId $ timeline ++ rest)
    where rest = take (maxTime - (length timeline)) $ repeat $ opposite
          opposite = (1 + (last timeline)) `mod` 2

combine :: Record -> Record -> Record
combine (Record id timeline1) (Record _ timeline2) = (Record id (zipWith (+) timeline1 timeline2))

compareSleepiestMinute :: Record -> Record -> Ordering
compareSleepiestMinute (Record _ time1) (Record _ time2) = (maximum time1) `compare` (maximum time2)

mostSleptMinute :: Record -> Int
mostSleptMinute (Record _ timeline) = fst $ maximumBy (\(x1,x2) (y1,y2) -> compare x2 y2) $ zip [0..] timeline

parseTime :: String -> Int
parseTime log
    | isInfixOf "23:" log = 0
    | otherwise = (read (head $ tail $ splitOn ":" time) ::Int)
    where time = (head $ tail $ words log) \\ "]"

parseRecords :: [Record] -> [(Int,LogType)] -> [Record]
parseRecords (record:records) [] = (complete record:records)
parseRecords [] ((guardId,_):logs) = parseRecords [newRecord guardId] logs
parseRecords records ((logValue, logType):logs)
    | logType == Awake  = parseRecords (record `appendTime` timeInactive:(tail records))        logs
    | logType == Asleep = parseRecords (record `appendTime` timeActive:(tail records))          logs
    | otherwise         = parseRecords ((newRecord logValue):(complete record):(tail records))  logs
    where record        = head records
          timeActive    = take elapsedTime $ awakes
          timeInactive  = take elapsedTime $ asleeps
          elapsedTime   = logValue - (totalTime record)

main :: IO ()
main = do
    input <- (readFile "../input")
    let logs = map parseLog $ sort $ lines input
    let records = parseRecords [] logs
    let guardSummaries = map (foldl1 combine) $ group $ sort records
    let sleepiestGuard = maximumBy (compareSleepiestMinute) guardSummaries
    let (Record guardId _) = sleepiestGuard
    print $ guardId * (mostSleptMinute sleepiestGuard)