import Data.List

mergeSplitted :: ([a], [a]) -> Maybe [a]
mergeSplitted (_,[]) = Nothing
mergeSplitted (prev, (x:rest)) = Just $ x : merge (reverse prev) rest where
    merge xs [] = xs
    merge [] ys = ys
    merge (x:xs) (y:ys) = (x:y:merge xs ys)

findNearest :: (a -> Bool) -> Int -> [a] -> Maybe a
findNearest p n xs = mergeSplitted (splitAt n xs) >>= find p

main = do
    print $ findNearest (>100) 4 [200,1,150,2,4] == Just 150
    print $ findNearest (<2) 4 [200,1,150,2,4] == Just 1
    print $ findNearest (>1) 4 [200,1,150,2,4] == Just  4
    print $ findNearest (>1000) 4 [200,1,150,2,4] == Nothing
    print $ findNearest (>100) 5 [200,1,150,2,4] == Nothing
