import Data.List


--lookAndSayPrec xs = lookAndSayPrec' xs >>= return . concat

lookAndSayPrec [] = []
lookAndSayPrec (x:y:xs) =
    ((replicate x y) : lookAndSayPrec xs)

main = do
    print $ lookAndSayPrec [4,1,3,7]
