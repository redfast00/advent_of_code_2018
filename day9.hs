import Data.List
import Control.Monad

import Control.Monad.State.Lazy (State)
import qualified Control.Monad.State.Lazy as State

type Marble = Int
type Score = Marble
type Player = Int

data MarbleState = MarbleState { scorelist :: [Score]
                               , currentplayer :: Player
                               , circle :: [Marble]
                               , currentmarble :: Marble
                               , current :: Marble
                               , numPlayers :: Int
                               } deriving (Show)

keepUntil :: Eq a => a -> [a] -> [a]
keepUntil k (x:xs)
    | k == x = []
    | otherwise = x:(keepUntil k xs)

insertAfter :: Eq a => a -> [a] -> a -> [a]
insertAfter after (x:xa:xb:rest) toAdd
    | x == after = x:xa:toAdd:xb:keepUntil x rest
    | otherwise  = insertAfter after (xa:xb:rest) toAdd

normalMarble :: State MarbleState ()
normalMarble = do
    state <- State.get
    let newcircle = (if (currentmarble state) < 7 then nub else id) (insertAfter (current state) (cycle $ circle state) (currentmarble state))
    let newcurrent = currentmarble state
    let newcurrentmarble = currentmarble state + 1
    let newcurrentplayer = (currentplayer state + 1) `mod` (numPlayers state)
    State.put $! state {circle=newcircle, current=newcurrent, currentmarble=newcurrentmarble,currentplayer=newcurrentplayer}

findsevencounterclockwise :: Eq a => [a] -> a -> a
findsevencounterclockwise (x:x1:x2:x3:x4:x5:x6:x7:rest) tofind
    | x7 == tofind = x
    | otherwise    = findsevencounterclockwise (x1:x2:x3:x4:x5:x6:x7:rest) tofind

findnext :: Eq a => [a] -> a -> a
findnext (x:x':xs) tofind
    | x == tofind = x'
    | otherwise = findnext (x':xs) tofind

flattenCircular :: Eq a => [a] -> [a]
flattenCircular (x:xs) = x : keepUntil x xs

scoreIncrease :: Player -> Score -> [Score] -> [Score]
scoreIncrease player inc scores = scoreIncrease' player inc scores 0
    where scoreIncrease' player inc (cur:rest) num
            | player == num = (cur + inc):rest
            | otherwise     = cur : scoreIncrease' player inc rest (num + 1)

specialMarble :: State MarbleState ()
specialMarble = do
    state <- State.get
    let scoreInc = currentmarble state
    let toRemove = findsevencounterclockwise (cycle $ circle state) (current state)
    let newCurrent = findnext (cycle $ circle state) toRemove
    let newcircle = delete toRemove (circle state)
    let newscores = scoreIncrease (currentplayer state) (scoreInc + toRemove) (scorelist state)
    let newcurrentmarble = currentmarble state + 1
    let newcurrentplayer = (currentplayer state + 1) `mod` (numPlayers state)
    State.put $! state{scorelist=newscores, circle=newcircle, current=newCurrent, currentmarble=newcurrentmarble, currentplayer=newcurrentplayer}


marble :: State MarbleState ()
marble = do
    state <- State.get
    if (currentmarble state) `mod` 23 == 0 then specialMarble else normalMarble
    return ()

main = do
    let numPlayers = 435
    let ascorelist = replicate numPlayers 0
    let currentplayer = 0
    let acircle = [0]
    let currentmarble = 1
    let current = 0
    let (ns, r) = State.runState (replicateM (71184) marble) (MarbleState ascorelist currentplayer acircle currentmarble current numPlayers)
    print $ (maximum $ scorelist r)
