-- http://lpaste.net/revision/8348335358680760320
window :: Int -> Int -> [a] -> [[a]]
window _ _ [] =  []
window size step xs = take size xs : window size step (drop step xs)

main = do
    print $ window 2 3 [0..9]
    print $ window 2 1 [0..9]
