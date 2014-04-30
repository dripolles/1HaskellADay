-- http://lpaste.net/revision/8348335358680760320
window :: Int -> Int -> [a] -> [[a]]
window _ _ [] =  []
window size step l = [take size l] ++ (window size step (drop step l))

main = do
    print $ window 2 3 [0..9]
    print $ window 2 1 [0..9]
