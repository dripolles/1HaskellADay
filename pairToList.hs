-- See http://codepad.org/gXEcCgBI
import Control.Applicative
 
pairToList :: (a,a) -> [a]
pairToList = (:) <$> fst <*> replicate 1 . snd
 
main = print $ pairToList (1,2)