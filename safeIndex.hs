-- http://lpaste.net/revision/8028035114564845568
(?!) :: [a] -> Int -> Maybe a
(?!) [] _ = Nothing
(?!) (x:xs) n
    | n < 0 = Nothing
    | n == 0 = Just x
    | otherwise = (?!) xs (n-1)

main = do
    print $ [0..3] ?! 2
    print $ [0..3] ?! 4
    print $ [0..] ?! 1000000
    print $ [0..3] ?! (-2)

