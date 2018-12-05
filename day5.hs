import Data.Char

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

main = do
    content <-readFile "inputs/input5.txt"
    let parsed = head $ lines content
    -- print $ lenght $ converge destroy parse
    let infos = (flip zip) ['a'..'z'] (fmap length $ fmap (converge destroy) $ fmap (`remove` parsed) ['a'..'z'])
    print infos
    print $ minimum infos

remove :: Char -> String -> String
remove chr = filter (\x -> x /= chr && toLower x /= chr)

matches :: Char -> Char -> Bool
matches x y
    | toLower x /= toLower y = False
    | x == y = False
    | otherwise = True

destroy :: String -> String
destroy (y:y':rest) = if matches y y' then destroy rest else y:(destroy (y':rest))
destroy [] = []
destroy [x] = [x]
