{-# LANGUAGE BangPatterns #-}

import Text.Parsec  hiding ((<|>), many, State)
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator

import Data.Char
import Data.Either
import Data.List
import Data.Maybe

import Control.Monad (msum)
import Control.Applicative

import Data.Map (Map)
import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad.State.Lazy (State)
import qualified Control.Monad.State.Lazy as State


data Pot = Dead | Alive deriving (Show, Eq, Ord)

data Rule = Rule       { ll :: Pot
                       , l  :: Pot
                       , c  :: Pot
                       , r  :: Pot
                       , rr :: Pot
                       , result :: Pot
                       } deriving (Show)
type Board = Map.Map Int Pot

parsePot :: Parser Pot
parsePot = deadplant <|> aliveplant
    where deadplant   = char '.' >> return Dead
          aliveplant  = char '#' >> return Alive

parseBoard :: Parser Board
parseBoard = do
    _ <- string "initial state: "
    potlist <- many parsePot
    _ <- char '\n'
    _ <- char '\n'
    return $ Map.fromList (zip [0..] potlist)

parserule :: Parser Rule
parserule = do
    ll <- parsePot
    l  <- parsePot
    c  <- parsePot
    r  <- parsePot
    rr <- parsePot
    _ <- string " => "
    result <- parsePot
    _ <- char '\n'
    return $ Rule ll l c r rr result

inputParse :: Parser (Board, [Rule])
inputParse = do
    board <- parseBoard
    rules <- many parserule
    return (board, rules)

ruleMatches :: Pot -> Pot -> Pot -> Pot -> Pot -> Rule -> Maybe Pot
ruleMatches ll l c r rr (Rule lla la ca ra rra result)
    | rr == rra && r == ra && c == ca && l == la && ll == lla = Just result
    | otherwise                                               = Nothing

getBoard :: Board -> Int -> Pot
getBoard board location = Map.findWithDefault Dead location board

getNext :: [Rule] -> Board -> Int -> Pot
getNext rules board location = fromJust $ (msum $ fmap (ruleMatches ll l c r rr) rules) <|> Just Dead
    where ll = getBoard board (location - 2)
          l  = getBoard board (location - 1)
          c  = getBoard board location
          r  = getBoard board (location + 1)
          rr = getBoard board (location + 2)


nextBoard :: [Rule] -> Board -> Board
nextBoard rules !board = nextBoard
    where updateLocations = Set.toList $ Set.fromList $ concatMap (\l -> [l-2..l+2]) $ Map.keys board
          nextBoard = Map.fromList $ filter ((== Alive) . snd) $ zip updateLocations (fmap (getNext rules board) updateLocations)


doNtimes :: a -> (a -> a) -> Int -> a
doNtimes !a f n = if n == 0 then a else doNtimes (f a) f (n - 1)

type CanonicalBoard = (Board, Int)

toCanonical :: Board -> CanonicalBoard
toCanonical board = (Map.mapKeys (\l -> l - offset) board, offset)
    where (offset, v) = Map.findMin board

score :: Board -> Int
score board = Map.foldlWithKey updatefunc 0 board
    where updatefunc acc k v = case v of
            Dead  -> acc
            Alive -> k + acc

parseFile = do
    content <- readFile "inputs/input12.txt"
    let Right (board, rules) = runParser inputParse () "" content
    return (board, rules)

first = do
    (board, rules) <- parseFile
    let lastBoard = doNtimes board (nextBoard rules) 20
    print $ score lastBoard

findDuplicate :: Map.Map Board (Int, Int) -> [(Int, CanonicalBoard)] -> (Int, Int, Int, Int)
findDuplicate maps [] = error "wtf"
findDuplicate maps ((index, (board, offset)):xs) = case Map.lookup board maps of
    Nothing                    -> findDuplicate (Map.insert board (index, offset) maps) xs
    Just (oldindex, oldoffset) -> (offset, index, oldoffset, oldindex)

score' :: Int -> Board -> Int
score' offset board = Map.foldlWithKey updatefunc 0 board
    where updatefunc acc k v = case v of
            Dead  -> acc
            Alive -> k + acc + offset

second = do
    (board, rules) <- parseFile
    let amt = 50000000000
    let (offset, index, oldoffset, oldindex) = findDuplicate Map.empty $ zip [0..] $ toCanonical <$> iterate (nextBoard rules) board
    let modded = fromIntegral $ (amt - oldindex) `mod` (index - oldindex)
    let amtskipped = fromIntegral $ (amt - oldindex) `div` (index - oldindex)
    print $ score' (amtskipped * (offset - oldoffset)) $ doNtimes board (nextBoard rules) (oldindex + modded)
