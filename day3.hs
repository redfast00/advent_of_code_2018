import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Char
import Data.Either


nat :: Parser Int
nat = do
    digits <- many1 digit
    return $ read digits

claimParser :: Parser Claim
claimParser = do
    _ <- char '#'
    identifier <- nat
    _ <- char ' '
    _ <- char '@'
    _ <- char ' '
    leftOffset <- nat
    _ <- char ','
    topOffset <- nat
    _ <- char ':'
    _ <- char ' '
    width <- nat
    char 'x'
    height <- nat
    return $ Claim identifier leftOffset topOffset width height

data Claim = Claim {
    identifier :: Int,
    leftOffset :: Int,
    topOffset  :: Int,
    width :: Int,
    height :: Int
} deriving (Eq, Show)

type Coordinate = (Int, Int)

main = do
    content <- readFile "inputs/input3.txt"
    let l = lines content
    let parsed = fmap (runParser claimParser () "input3.txt") l
    let correctParsed = rights parsed
    let areas = concatMap getArea correctParsed
    let countsMap = countAll Map.empty areas
    print $ length $ filter ((2 <=) . snd) $ Map.toList countsMap
    print $ identifier $ head $ filter (intact countsMap) correctParsed

countAll :: Map.Map Coordinate Int -> [Coordinate] -> Map.Map Coordinate Int
countAll seen [] = seen
countAll seen (coord:todo) = case Map.lookup coord seen of
    Nothing  -> countAll (Map.insert coord 1 seen) todo
    (Just c) -> countAll (Map.insert coord (c + 1) seen) todo

getArea :: Claim -> [Coordinate]
getArea claim = [(x, y) | x <- [(topOffset claim)..(topOffset claim + height claim)-1], y <- [(leftOffset claim)..(leftOffset claim + width claim)-1]]

intact :: Map.Map Coordinate Int -> Claim -> Bool
intact seen claim = all once (getArea claim)
    where once coord = Map.findWithDefault 1 coord seen == 1
