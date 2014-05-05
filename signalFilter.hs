-- http://lpaste.net/revision/2812040132369055744
signalFilter :: [Bool] -> [a] -> [Maybe a]
signalFilter = zipWith (\p x -> if p then Just x else Nothing)

main = do
    print $ signalFilter [True,False,True] "hello"
