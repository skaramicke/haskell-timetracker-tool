module Main where


import Data.Text ()
import Data.List
import Control.Monad ( unless )
import qualified Data.ByteString as S
import Control.Lens.Internal.ByteString ( unpackStrict8 )
import Data.Time ( UTCTime, getCurrentTime )
import Data.Time.Format ( formatTime, defaultTimeLocale )
import Data.Time.Clock.POSIX ( utcTimeToPOSIXSeconds, posixSecondsToUTCTime )
import Data.ByteString.Char8 ( pack )
import System.IO ( IOMode(ReadMode), IOMode(WriteMode), hPutStr, withFile )
import System.Environment ( getArgs )
import System.Directory ( doesFileExist )


-- State type stuff
newtype State = State { timers :: [Timer] } deriving (Show, Read, Eq)

data Timer = Timer { timerIdentifier :: String, events :: [ Int ] } deriving (Show, Read, Eq)

-- The world type, used in returns in the outer pure layer
type World = (State, Maybe String)


-- global stuff
filename :: FilePath 
filename = "timetracker.state"

secondsSinceEpoch :: UTCTime -> Int
secondsSinceEpoch = floor . utcTimeToPOSIXSeconds

main :: IO ()
main = do
  -- Load state
  state <- loadState

  -- Read input parameters
  args <- getArgs

  -- Get curren time
  u <- getCurrentTime
  let time = secondsSinceEpoch u
  
  -- Run the pure part of the program
  let (newState, output) = run state time args

  -- put state back in the file
  saveState newState

  -- produce output for user
  case output of
    Just s -> Prelude.putStrLn s




-- State handlers
loadState :: IO State
loadState = do

  exists <- doesFileExist filename
  unless exists $ saveState $ State []

  contents <- S.readFile filename
  let state =  read (unpackStrict8 contents) :: State
  return state

saveState :: State -> IO ()
saveState s = do
  S.writeFile filename $ pack $ show s


-- Pure entry point
run :: State -> Int -> [String] -> World

run state time [] = -- No arguments, print what timers are running
  let activeTimers = filter (odd.length.events) (timers state)
      names = map timerIdentifier activeTimers
      eventLists = map events activeTimers
      startTimes = map last eventLists
  in case names of
    [] -> (state, Just "Not tracking")
    [x] -> (state, Just (
      x ++ ": " ++ timeStringFromSeconds (timeBetweenTimestamps (last startTimes) time)))
    x -> (state, Just ("Currently tracking " ++ commalist (quoteall x) ++ " ... eventhough there should be only one"))
    
run state time [arg] = -- Single argument
  case arg of
    "start" -> startTimer time "generic" state
    "stop" -> stopAllTimers time state
    "report" -> report time state
    "clear" -> (state, Just "Are you sure you want to clear all projects? Let me know using --for-sure")
    arg -> (state, Just $ "I don't know how to " ++ arg)

run state time (arg:args) =
  case arg of
    "start" -> startTimer time (Prelude.unwords args) state
    "stop" -> stopAllTimers time state
    "clear" -> case firstElement args of
                  Just "--for-sure" -> (state, Just "removing EVERYTHING!!1 but not really becuse it's not implemented yet")
                  Just timerName -> (state, Just ("removing "++timerName++" but not really becuse it's not implemented yet"))
    arg -> (state, Just $ "I don't know how to " ++ arg ++ Prelude.unwords args)


firstElement :: [a] -> Maybe a
firstElement [] = Nothing
firstElement (x:xs) = Just x

commalist :: [String] -> String 
commalist [] = []
commalist [x] = x
commalist [x, y] = x ++ " and " ++ y
commalist (x:xs) = x ++ ", " ++ commalist xs

quoteall :: [String] -> [String]
quoteall = map (\s -> "'" ++ s ++ "'")


startTimer :: Int -> String -> State -> World
startTimer time timerName state =
  case filter (\t -> timerIdentifier t == timerName) (timers state) of
    [] -> createTimer time timerName state
    x -> ( state { timers = startMatchingTimers time (timers state) timerName } , Just ("Started tracking " ++ timerName) )


stopNonMatchingRunningTimers :: Int -> [Timer] -> String -> [Timer]
stopNonMatchingRunningTimers _ [] _ = []
stopNonMatchingRunningTimers time timers identifier = map
  (\t ->
    if odd (length (events t)) && timerIdentifier t /= identifier
      then t { events = events t ++ [time] }
      else t
  ) timers


startMatchingTimers :: Int -> [Timer] -> String -> [Timer]
startMatchingTimers _ [] _ = []
startMatchingTimers time timers identifier = map
  (\t ->
    if even (length (events t)) && timerIdentifier t == identifier
      then t { events = events t ++ [time] }
      else t
  ) (stopNonMatchingRunningTimers time timers identifier)


createTimer :: Int -> String -> State -> World
createTimer time timerName state =
  ((state { timers = stopNonMatchingRunningTimers time (timers state) timerName ++ [Timer timerName [time]] }), Just ("Started tracking " ++ timerName))
  

stopAllTimers :: Int -> State -> World
stopAllTimers time state =
  ( state { timers = map
  (\t ->
    if odd (length (events t))
      then t { events = events t ++ [time] }
      else t
  ) (timers state) }, Just "Stopped tracking time" )


pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [x] = []
pairs [x, y] = [(x, y)]
pairs (x:y:xs) = (x, y) : pairs xs

-- pairs, but add "evener" int to any last odd element
pairs' :: [Int] -> Int -> [(Int, Int)]
pairs' [] evener = []
pairs' [x] evener = [(x, evener)]
pairs' [x, y] evener = [(x, y)]
pairs' (x:y:xs) evener = (x, y) : pairs' xs evener

report :: Int -> State -> World
report time state =
  (state, Just ("== Report ==" ++ unwords (reportString (timers state) time)))

reportString :: [Timer] -> Int -> [String]
reportString [] _ = ["\n- Nothing to report"]
reportString timers time = map (
  \t -> 
    "\n\nProject: " ++ timerIdentifier t
    ++ unwords (map (
      \(a, b) -> "\n" ++ datetimeFromSeconds a ++ " - " ++ datetimeFromSeconds b ++ " = " ++ timeStringFromSeconds (timeBetweenTimestamps a b)
    ) (pairs' (events t) time)
  ) ++ "\nTotal: " ++ timeStringFromSeconds (sumEvents (events t) time)) timers


sumEvents :: [Int] -> Int -> Int
sumEvents [] time = 0
sumEvents l time = sum (map timeBetweenPair (pairs' l time))

timeBetweenPair :: (Int, Int) -> Int
timeBetweenPair (a, b) = timeBetweenTimestamps a b

timeBetweenTimestamps :: Int -> Int -> Int
timeBetweenTimestamps a b = b - a

timeStringFromSeconds :: Int -> String
timeStringFromSeconds s = commalist (timeListFromSeconds s)

timeListFromSeconds :: Int -> [String]
timeListFromSeconds s
  | s >= 86400 = pluralize (s `div` 86400) "day" "days" : timeListFromSeconds (s - (s `div` 86400) * 86400)
  | s >= 3600 = pluralize (s `div` 3600) "hour" "hours" : timeListFromSeconds (s - (s `div` 3600) * 3600)
  | s >= 60 = pluralize (s `div` 60) "minute" "minutes" : timeListFromSeconds (s - (s `div` 60) * 60)
  | s > 0 = [pluralize s "second" "seconds"]
  | otherwise = []

pluralize :: Int -> String -> String -> String 
pluralize 1 w _ = "1 " ++ w
pluralize n _ w = show n ++ " " ++ w

datetimeFromSeconds :: Int -> String
datetimeFromSeconds = formatTime defaultTimeLocale "%F %X" . posixSecondsToUTCTime . fromIntegral
