-- http://lpaste.net/revision/7041447080467890176
import Control.Applicative
 
braid :: [a] -> [a] -> [a]
braid xs ys = zip xs ys >>= (:) <$> fst <*> return . snd
 
main = do
    print $ braid [0,2] [1,3 ..]