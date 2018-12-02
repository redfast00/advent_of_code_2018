import Debug.Trace

main = do
    content <- readFile "inputs/input1.txt"
    let parsed = parse $ lines content
    print $ sum parsed
    print $ solveSecond (cycle parsed) [0] 0

parse :: [String] -> [Int]
parse = fmap parseSingle

parseSingle :: String -> Int
parseSingle ('+':rest) = read rest
parseSingle ('-':rest) = negate (read rest)

solveSecond :: [Int] -> [Int] -> Int -> Maybe Int
solveSecond [] _ _ = Nothing
solveSecond (x:xs) reached acc = if next `elem` reached then
        Just next
    else
        solveSecond xs (next:reached) next
    where next = x + acc
