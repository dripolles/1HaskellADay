import Data.List

-- $setup
-- >>> import Test.QuickCheck

{- | list the n-grams of size n of the given entry list.

   Properties:

   prop> \(NonEmpty xs) -> (concat $ nGrams 1 xs) == xs
   prop> \(Positive n, NonEmpty xs) -> (length $ nGrams n xs) == max 1 ((length xs) - n + 1)

   Example:

   >>> nGrams 3 "hello"
   ["hel,"ell","llo"]
-}

nGrams :: Int -> [a] -> [[a]]
nGrams n = unfoldr go where
    go [] = Nothing
    go xs = return (take n xs, shortTail) where
        shortTail =
            let ts = tail xs
            in if length (take n ts) == n then ts else []
    
main = do
    print $ nGrams 3 [1..10]
    print $ take 5 $ nGrams 4 [1..]
    print $ nGrams 10 [1..3]
