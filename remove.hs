import Data.List

remove :: Int -> [a] -> [[a]]
remove n xs = map selectPairedWithTrue combinations where
    selectPairedWithTrue = (map fst) . (filter snd) . (zip xs)
    combinations = permutations $ take (length xs) $ replicate n False ++ repeat True

main = do
    print $ remove 2 "abcde"
