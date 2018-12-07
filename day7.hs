import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator

import Data.Char
import Data.Either
import Data.List
import Data.Maybe

--import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad.State.Lazy (State)
import qualified Control.Monad.State.Lazy as State

import Debug.Trace

type Step = Char

data Dependency = Depends {from :: Step, to :: Step} deriving (Show, Eq)

newtype Worker = Worker {job :: Maybe (Step, Time)} deriving (Eq, Ord, Show)
type Time = Int
data WorkerState = WorkerState { workers :: [Worker]
                               , dependencies :: Map.Map Step [Step]
                               , incomingCount :: Map.Map Step Int
                               , used ::  Set.Set Step
                               , currentTime :: Time
                               }


parseDependency :: Parser Dependency
parseDependency = do
    _ <- string "Step "
    f <- letter
    _ <- string " must be finished before step "
    t <- letter
    _ <- string " can begin."
    return $ Depends f t

findZero :: Map.Map Step Int -> Maybe Step
findZero hmap = listToMaybe $ sort $  Map.keys $  Map.filter (==0) hmap

decreaseIncoming :: Map.Map Step Int -> Map.Map Step [Step] -> Step -> Map.Map Step Int
decreaseIncoming incomingAmount steps step = case Map.lookup step steps of
    Nothing -> Map.delete step incomingAmount
    (Just lst) -> Map.delete step $ foldl (flip (Map.adjust (\ x -> x - 1))) incomingAmount lst

solveDependency :: Map.Map Step Int -> Map.Map Step [Step] -> [Step]
solveDependency incomingAmount dependencies = case findZero incomingAmount of
    Nothing -> []
    (Just val) -> val : solveDependency (decreaseIncoming incomingAmount dependencies val) dependencies

countIncoming :: [Dependency] -> Map.Map Step Int
countIncoming [] = Map.empty
countIncoming (Depends from to : todo) = Map.insertWith (+) to 1 (Map.insertWith (+) from 0 (countIncoming todo))

mapIncoming :: [Dependency] -> Map.Map Step [Step]
mapIncoming [] = Map.empty
mapIncoming (Depends from to : todo) = Map.insertWith (++) from [to] (mapIncoming todo)

cost :: Step -> Int
cost step = ord step - ord 'A' + 1 + fixed

input :: IO [Dependency]
input = do
    content <- readFile "inputs/input7.txt"
    let l = lines content
    return $ rights $ fmap (runParser parseDependency () "") l

first = do
    parsed <- input
    print $ solveDependency (countIncoming parsed) (mapIncoming parsed)

findZeroUnassigned :: Map.Map Step Int -> Set.Set Step -> Maybe Step
findZeroUnassigned hmap assigned = Set.lookupMin (Set.difference zeros assigned)
    where zeros = Set.fromList $ Map.keys $ Map.filter (==0) hmap

checkDone :: Time -> Worker -> Maybe Step
checkDone _ (Worker Nothing) = Nothing
checkDone time (Worker (Just (step, donetime)))
    | donetime <= time = Just step
    | otherwise        = Nothing

removeStep :: Step -> State.State WorkerState ()
removeStep step = do
    curstate <- State.get
    State.put $ curstate{incomingCount = decreaseIncoming (incomingCount curstate) (dependencies curstate) step}

assignWorker :: State.State WorkerState Bool
assignWorker = do
    WorkerState {workers=workers, used = set, currentTime=time, incomingCount = incomingCount} <- State.get
    case job $ head workers of
        (Just _) -> return True
        Nothing -> case findZeroUnassigned incomingCount set of
            Nothing -> return True
            (Just val) -> do
                let newWorkers = tail workers ++ [Worker (Just (val, time + cost val))]
                State.modify (\s -> s{workers=newWorkers, used=Set.insert val set})
                return False

whileM :: Monad m => m Bool -> m ()
whileM action = do
    finished <- action
    if finished then return () else whileM action

step :: State.State WorkerState Bool
step = do
    -- update workers
    WorkerState{workers = workers, currentTime = time}  <- State.get
    let finishedSteps = mapMaybe (checkDone time) workers
    let newWorkers = sort $ fmap (\worker -> if isJust $ checkDone time worker then Worker Nothing else worker) workers
    State.modify (\s -> s{workers=newWorkers})

    mapM_ removeStep finishedSteps
    whileM assignWorker
    State.modify (\workerstate -> workerstate{currentTime = time + 1})
    WorkerState{workers=workers} <- State.get
    return $ not $ any (isJust . job) workers

secondState :: State.State WorkerState Int
secondState = do
    whileM step
    currentTime <$> State.get

fixed, workerAmount :: Int
fixed = 60
workerAmount = 5

second = do
    parsed <- input
    let workers = replicate workerAmount (Worker Nothing)
    let incomingCount = countIncoming parsed
    let deps = mapIncoming parsed
    let used = Set.empty
    print $ fst $ State.runState secondState (WorkerState workers deps incomingCount used (-1))
