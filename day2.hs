import Control.Applicative

main = do
    content <- readFile "inputs/input2.txt"
    let labels = lines content
    let twos = length $ filter id $ fmap (hasNSame 2 []) labels
    let threes = length $ filter id $ fmap (hasNSame 3 []) labels
    print $ twos * threes
    let combos = [(x,y) | x <- labels, y <- labels, x /= y]
    putStrLn $ head $ uncurry same <$> filter (uncurry max1diff) combos

hasNSame :: Eq a => Int -> [a] -> [a] -> Bool
hasNSame amt forbidden [] = False
hasNSame amt forbidden (x:xs)
    | x `elem` forbidden     = hasNSame amt forbidden xs
    | amount xs x == amt - 1 = True
    | otherwise              = hasNSame amt (x:forbidden) xs

amount :: Eq a => [a] -> a -> Int
amount [] _ = 0
amount (x:xs) s = if x == s then 1 + rest else rest
    where rest = amount xs s

max1diff :: Eq a => [a] -> [a] -> Bool
max1diff [] _ = False
max1diff _ [] = False
max1diff (x:xs) (y:ys)
    | x /= y    = xs == ys
    | otherwise = max1diff xs ys

same :: Eq a => [a] -> [a] -> [a]
same xs ys = fst <$> filter (uncurry (==)) (zip xs ys)
