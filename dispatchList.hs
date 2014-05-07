-- http://lpaste.net/revision/3548769504878133248
import Control.Arrow

dispatchList :: [a] -> ([a], [a])
dispatchList = (f id &&& f not) . zip (cycle [True, False])
    where f p = map snd . (filter $ p . fst)    

main = do
    print $ dispatchList [1..5]
