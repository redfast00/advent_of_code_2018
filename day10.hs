import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator

import Data.Char
import Data.Either
import Data.List
import Data.Maybe

import Control.Monad (replicateM)
import Control.Applicative hiding ((<|>))

import Data.Set (Set)
import qualified Data.Set as Set

data Coord = Coord Int Int deriving (Show, Eq, Ord)

data Point = Point {location :: Coord, velocity :: Coord} deriving (Show, Eq, Ord)

(>+) :: Coord -> Coord -> Coord
(Coord x y) >+ (Coord x' y') = Coord (x + x') (y + y')


integer :: Parser Int
integer = rd <$> (plus <|> minus <|> number)
    where rd     = read :: String -> Int
          plus   = char '+' *> number
          minus  = (:) <$> char '-' <*> number
          number = many1 digit

parseCoordinate :: Parser Coord
parseCoordinate = do
    _ <- char '<'
    _ <- spaces
    f <- integer
    _ <- string ","
    _ <- spaces
    s <- integer
    _ <- char '>'
    return $ Coord f s


-- position=<-41150,  41504> velocity=< 4, -4>
parsePoint :: Parser Point
parsePoint = do
    _ <- string "position="
    pos <- parseCoordinate
    _ <- string " velocity="
    vel <- parseCoordinate
    return $ Point pos vel

next :: Point -> Point
next (Point pos vel) = Point (pos >+ vel) vel

nextAll :: [Point] -> [Point]
nextAll = fmap next

nextAm' :: Int -> Point -> Point
nextAm' amt (Point pos vel@(Coord x y)) = Point (pos >+ Coord (x * amt) (y * amt)) vel

nextAmt :: [Point] -> Int -> [Point]
nextAmt points amt = fmap (nextAm' amt) points

average :: [Int] -> Int
average xs = sum xs `div` length xs

sumSquares :: [Point] -> Int
sumSquares points = xsquare + ysquare
    where xavg = average $ fmap ((\(Coord a b) -> a) . location) points
          yavg = average $ fmap ((\(Coord a b) -> b) . location) points
          xsquare = sum $ fmap ((\(Coord a b) -> (a - xavg)^2 ) . location) points
          ysquare = sum $ fmap ((\(Coord a b) -> (b - yavg)^2 ) . location) points

display :: [Coord] -> [String]
display points = visual
    where pointset = Set.fromList points
          maxx = maximum $ fmap (\(Coord a b) -> a) points
          maxy = maximum $ fmap (\(Coord a b) -> b) points
          minx = minimum $ fmap (\(Coord a b) -> a) points
          miny = minimum $ fmap (\(Coord a b) -> b) points
          visual = [[if Set.member (Coord b a) pointset then '#' else '.' | b <- [minx..maxx]] | a <- [miny..maxy]]

parseInput :: IO [Point]
parseInput = do
    content <- readFile "inputs/input10.txt"
    let l = lines content
    let content = rights $ fmap (runParser parsePoint () "") l
    return content

first = do
    content <- parseInput
    let range = [0,100..1000000]
    let (_, subrange) = minimum $ zip (fmap (sumSquares . nextAmt content) range) range
    let secrange = [(subrange - 100)..(subrange + 100)]
    let (_, value) = minimum $ zip (fmap (sumSquares . nextAmt content) secrange) secrange
    putStr $ unlines $ display $ fmap location $ iterate nextAll content !! value
    print value
