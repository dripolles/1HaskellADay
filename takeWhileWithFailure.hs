-- http://lpaste.net/revision/7250884297299591168
import Control.Applicative
import Control.Monad

takeWhileWithFailure :: (a -> Bool) -> [a] -> [a]
takeWhileWithFailure f l = 
    map snd $ takeWhile (f . fst) $ zip (head l:l) l

takeWhileWithFailure' :: (a -> Bool) -> [a] -> [a]
takeWhileWithFailure' f =
    map snd .
    takeWhile (f . fst) . (zip <$> join ((:) . head) <*> id)

main = do
    print $ takeWhileWithFailure (<3) [1..4]
    print $ takeWhileWithFailure' (<3) [1..4]
