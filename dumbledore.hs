import Control.Applicative
import Data.Tree

dumbledore :: (a -> [b] -> b) -> Tree a -> b
dumbledore f (Node x xs) = f x $ map (dumbledore f) xs

height :: Tree a -> Int
height = dumbledore $ \_ xs -> 1 + maximum (0:xs)

height' :: Tree a -> Int
height' = dumbledore $ const $ (1+) <$> maximum . (0:)

main = do
    print $ height $ Node 4 [Node 5 [], Node 2 [Node 3 []]]
    print $ height' $ Node 4 [Node 5 [], Node 2 [Node 3 []]]
