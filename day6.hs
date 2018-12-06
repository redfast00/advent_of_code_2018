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

nat :: Parser Int
nat = do
    digits <- many1 digit
    return $ read digits

dist p p' = (abs (d1 p - d1 p')) + (abs (d2 p - d2 p'))

data Coordinate = Coord {d1 :: Int, d2 :: Int} deriving (Show, Eq, Ord)

nearNeighbor' :: [Coordinate] -> Coordinate -> Maybe Coordinate
nearNeighbor' coords point = case filtered of
    [x] -> Just (snd x)
    _   -> Nothing
    where distances = fmap (\x -> (dist x point, x)) coords
          minDistance = minimum $ fmap fst distances
          filtered = filter (\x -> minDistance == fst x) distances

isSafe :: [Coordinate] -> Coordinate -> Bool
isSafe coords point = ((< 10000) . sum . (fmap (dist point))) coords

coordParse :: Parser Coordinate
coordParse = do
    x <- nat
    _ <- string ", "
    y <- nat
    return $ Coord x y

coordFinder :: [Coordinate] -> Int -> Int -> Int -> Int -> [[Maybe Coordinate]]
coordFinder tree minx maxx miny maxy = [[nearNeighbor' tree (Coord x y) | y <- [miny..maxy]] | x <- [minx..maxx]]

safetyChecker :: [Coordinate] -> Int -> Int -> Int -> Int -> [Coordinate]
safetyChecker tree minx maxx miny maxy = concat [[(Coord x y) | y <- [miny..maxy], isSafe tree (Coord x y)] | x <- [minx..maxx]]

countAll :: Map.Map Coordinate Int -> [Coordinate] -> Map.Map Coordinate Int
countAll seen [] = seen
countAll seen (coord:todo) = case Map.lookup coord seen of
    Nothing  -> countAll (Map.insert coord 1 seen) todo
    (Just c) -> countAll (Map.insert coord (c + 1) seen) todo

buffer = 20

firstpart = do
    content <- readFile "inputs/input6.txt"
    let l = lines content
    let parsed = rights $ fmap (runParser coordParse () "") l
    let minx = (minimum $ fmap d1 parsed) - buffer
    let maxx = (maximum $ fmap d1 parsed) + buffer
    let miny = (minimum $ fmap d2 parsed) - buffer
    let maxy = (maximum $ fmap d2 parsed) + buffer
    --print $ show minx ++ "," ++ show maxx ++ "," ++ show miny ++ "," ++ show maxy
    let found = coordFinder parsed minx maxx miny maxy
    let noAllowed = catMaybes $ head found ++ last found ++ fmap head found ++ fmap last found
    let areas = catMaybes $ concat found
    let countsMap = (countAll Map.empty areas) `Map.difference` (Map.fromList (fmap (\x -> (x, 0)) noAllowed))
    print countsMap
    let greatest = foldl1 (\ (g, t) (g', t') -> if t > t' then (g, t) else (g', t')) (Map.toList countsMap)
    print greatest

second = do
    content <- readFile "inputs/input6.txt"
    let l = lines content
    let parsed = rights $ fmap (runParser coordParse () "") l
    let minx = (minimum $ fmap d1 parsed) - buffer
    let maxx = (maximum $ fmap d1 parsed) + buffer
    let miny = (minimum $ fmap d2 parsed) - buffer
    let maxy = (maximum $ fmap d2 parsed) + buffer
    let safespace = safetyChecker parsed minx maxx miny maxy
    print $ length safespace
