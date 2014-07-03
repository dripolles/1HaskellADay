import Data.Maybe
import Data.List

matchingSub :: Int -> [Int] -> [[Int]]
matchingSub n xs = catMaybes $ map (matchingInit n) (tails xs) where
    matchingInit :: Int -> [Int] -> Maybe [Int]
    matchingInit n [] = if n == 0 then Just [] else Nothing
    matchingInit n (x:xs) = go (n-x) where
        go diff
            | diff == 0 = Just [x]
            | diff < 0 = Nothing
            | otherwise = matchingInit diff xs >>= return . (x:)

main = do
    print $ matchingSub 10 [0..10]
    print $ matchingSub 2 $ replicate 3 1
    print $ take 2 $ matchingSub 2 $ repeat 1
