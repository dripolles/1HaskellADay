-- http://codepad.org/4ZT5ZCwH
import Control.Applicative
import Control.Arrow
import Data.List
 
maximumList :: Ord a => [a] -> [a]
maximumList =  mapAccumL (\x -> max x &&& max x) <$> head <*> id >>> snd
 
-- Different approach
maximumList' :: Ord a => [a] -> [a]
maximumList' = groupBy (>=) >>> concatMap (map <$> const . head <*> id)
 
main = do
    let l = [1,2,1,1,3,4,2,2,10,9,8]
    print $ maximumList l
    print $ maximumList' l