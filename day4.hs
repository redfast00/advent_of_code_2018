import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator

import Data.Char
import Data.Either
import Data.List
import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as Map

type DateTime = (Int, Int, Int, Int, Int)
type GuardId = Int
data Action = FallAsleep | Wakeup | ChangeGuard GuardId
    deriving (Show, Eq)
data Event = Event Action DateTime
    deriving (Show, Eq)

instance (Ord Event) where
    compare (Event _ time) (Event _ time') = compare time time'

nat :: Parser Int
nat = do
    digits <- many1 digit
    return $ read digits

dateTimeParser :: Parser DateTime
dateTimeParser = do
    _ <- char '['
    year <- nat
    _ <- char '-'
    month <- nat
    _ <- char '-'
    day <- nat
    _ <- char ' '
    hour <- nat
    _ <- char ':'
    minute <- nat
    _ <- char ']'
    _ <- char ' '
    return (year, month, day, hour, minute)

beginShift :: Parser Action
beginShift = do
    _ <- string "Guard #"
    number <- nat
    _ <- string " begins shift"
    return $ ChangeGuard number

asleep :: Parser Action
asleep = do
    _ <- string "falls asleep"
    return FallAsleep

wakeup :: Parser Action
wakeup = do
    _ <- string "wakes up"
    return Wakeup

eventParser :: Parser Event
eventParser = do
    datetime <- dateTimeParser
    action <- beginShift <|> asleep <|> wakeup
    return $ Event action datetime

main = do
    content <- readFile "inputs/input4.txt"
    let l = lines content
    let parsed = fmap (runParser eventParser () "") l
    let correctParsed = sort $ rights parsed
    let cleaned = cleanerData' $ correctParsed ++ [Event (ChangeGuard 1337) (2000, 1, 1, 0, 0)]
    --print $ fmap (\(g, xs) -> (g, listToMaybe xs, length xs)) $ cleaned
    let hours = groupCleaned cleaned 0 (fst $ head cleaned, [])
    let grouped = foldl (\m (gard,lst) -> Map.insertWith (++) gard [lst] m) Map.empty hours
    let counts = Map.map (sum . fmap countBools) grouped
    let greatest = fst $ foldl1 (\ (g, t) (g', t') -> if t > t' then (g, t) else (g', t')) (Map.toList counts)
    print greatest
    let bins = sleepMinutes $ grouped Map.! greatest
    let maxval = maximum bins
    let sleep = elemIndex maxval bins
    let val = fromJust sleep
    print $ val * greatest

    let frequ = Map.map sleepMinutes grouped
    let (sleepGuard, sleeplist) = foldl1 (\ (g, t) (g', t') -> if maximum t > maximum t' then (g, t) else (g', t')) (Map.toList frequ)
    print sleepGuard
    let a' = fromJust $elemIndex (maximum sleeplist) sleeplist
    print $ a' * sleepGuard


sleepMinutes :: [[Bool]] -> [Int]
sleepMinutes q = foldl1 combine $ fmap boolToInt q

combine :: [Int] -> [Int] -> [Int]
combine a b = fmap (uncurry (+)) $ zip a b

boolToInt :: [Bool] -> [Int]
boolToInt = fmap (\x -> if x then 0 else 1)

countBools :: [Bool] -> Int
countBools [] = 0
countBools (False:rest) = 1 + countBools rest
countBools (True:rest) = countBools rest

groupCleaned :: [(GuardId, [Bool])] -> Int -> (GuardId, [Bool]) -> [(GuardId, [Bool])]
groupCleaned [] _ _ = []
groupCleaned ((guard, d):todo) prev (curguard, curd)
    | prev + length d == 60 = (curguard, curd ++ d):(groupCleaned todo 0 (0, []))
    | curguard /= guard && prev /= 0 = error "not supposed to happen"
    | otherwise = groupCleaned todo (prev + length d) (guard, curd ++ d)

cleanerData' :: [Event] -> [(GuardId, [Bool])]
cleanerData' events = tail $ cleanerData events 0 (0, 0, 0, 0, 0) True

shiftTime :: DateTime -> DateTime
shiftTime (year, month, day, hour, minute)
     | hour == 0 = (year, month, day, 0,  0)
     | otherwise = (year, month, day + 1, 0, 0)

cleanerData :: [Event] -> GuardId -> DateTime -> Bool -> [(GuardId, [Bool])]
cleanerData [] _ _  _ = []
cleanerData ((Event Wakeup t@(_, _, _, _, minute)):todo) g (_, _, _, _, minute') previous
    = (g, replicate (minute - minute') previous):(cleanerData todo g t True)
cleanerData ((Event FallAsleep t@(_, _, _, _, minute)):todo) g (_, _, _, _, minute') previous
    = (g, replicate (minute - minute') previous):(cleanerData todo g t False)
cleanerData ((Event (ChangeGuard guard) t):todo) g (_, _, _, _, minute') previous
    = (g, replicate (60 - minute') previous):(cleanerData todo guard (shiftTime t) True)
