import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator

import Control.Monad (replicateM)

import Debug.Trace

type Metadata = Int

data Node = Node [Node] [Metadata] deriving (Show)

nat :: Parser Int
nat = do
    digits <- many1 digit
    return $ read digits

treeParser :: Parser Node
treeParser = do
    childrenAmt <- nat
    _ <- char ' '
    metadataAmt <- nat
    children <- replicateM childrenAmt (char ' ' >> treeParser)
    metadata <- replicateM metadataAmt (char ' ' >> nat)
    return $ Node children metadata

sumOfNodes :: Node -> Int
sumOfNodes (Node children metadata) = sum (fmap sumOfNodes children) + sum metadata

parseInput :: IO Node
parseInput = do
    content <- readFile "inputs/input8.txt"
    let l = head $ lines content
    let (Right content) = runParser treeParser () "" l
    return content

value :: Node -> Int
value (Node [] metadata) = sum metadata
value (Node children metadata) = sum $ fmap getChildValue metadata
    where childrenlen = length children
          getChildValue index
             | 1 <= index && index <= childrenlen = value (children !! (index - 1))
             | otherwise = 0

first :: IO Int
first = sumOfNodes <$> parseInput

second :: IO Int
second = value <$> parseInput
