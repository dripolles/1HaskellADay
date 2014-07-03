intTuples :: [(Integer, Integer)]
intTuples = concat [
    if i == j then [(i,i)] else [(j,i), (i,j)]
        | i <- [1..], j <- [1..i]
    ]

main = do
    print $ take 50 $ intTuples
