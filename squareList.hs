-- http://codepad.org/rwuXZKKQ
import Data.List.Split
 
squareList :: Int -> a -> [a] -> [[a]]
squareList n z xs = let
    (x', xs') = splitAt n $ xs ++ repeat z
    in take n (x' : squareList n z xs')
 
squareList' :: Int -> a -> [a] -> [[a]]
squareList' n z xs = take n $ chunksOf n (xs ++ repeat z)
 
main = do
    print $ squareList 5 0 [1..19]
    print $ squareList' 5 0 [1..19]