-- http://lpaste.net/revision/110003157651685376
import Control.Applicative
import Data.List

maxTuple :: (Ord a, Num a) => [a] -> Maybe a
maxTuple (x:y:xs) = 
    Just . (liftA2 (+) fst snd) $ foldl' go (x,y) xs where
        go (x,y) z = let (x':y':_) = sortBy (flip compare) [x,y,z] in (x',y')
maxTuple _ = Nothing

main = do
    print $ maxTuple []
    print $ maxTuple [1]
    print $ maxTuple [1..5]
    print $ maxTuple [5,4,2,5,1]

