-- http://lpaste.net/revision/3166527577426755584
import Data.Monoid
 
replicateF :: Int -> (a -> a) -> a -> a
replicateF n = ((flip (!!) n . ) . iterate)
 
replicateF' :: Int -> (a -> a) -> a -> a
replicateF' n = appEndo . mconcat . replicate n . Endo
 
main = do
    print $ replicateF 10 (*2) 1
    print $ replicateF' 10 (*2) 1