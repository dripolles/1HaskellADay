-- http://lpaste.net/revision/8767529874781896704
reverseMap :: [a -> b] -> a -> [b]
reverseMap xs a = map ($ a) xs

reverseMap' :: [a -> b] -> a -> [b]
reverseMap' = flip $ map . flip ($)

main = do
    print $ reverseMap  [(+1), (*3)] 2
    print $ reverseMap' [(+1), (*3)] 2
