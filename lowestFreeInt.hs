-- http://lpaste.net/revision/5805301582249590784
import Control.Arrow
import Data.Set as S (Set, empty, insert, member)
 
lowestFreeInt :: [Int] -> Int
lowestFreeInt = fst . foldl f (0, S.empty) where
    f (n,s) x = (if (n == x) then nextFreeInt (n+1) s else n, S.insert x s)
 
lowestFreeInt' :: [Int] -> Int
lowestFreeInt' = fst . foldl f (0, S.empty) where
    f (n,s) x = nextFreeInt n &&& id $ S.insert x s
 
nextFreeInt :: Int -> S.Set Int -> Int
nextFreeInt n s = head $ dropWhile (flip S.member s) [n..]
 
main = do
    print $ lowestFreeInt $ [0..9] ++ [15,14..11]
    print $ lowestFreeInt $ [0..9] ++ [15,14..10]
    print $ lowestFreeInt' $ [0..9] ++ [15,14..11]
    print $ lowestFreeInt' $ [0..9] ++ [15,14..10]