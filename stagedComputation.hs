-- http://lpaste.net/revision/6908970897281908736
import Control.Category ((>>>))
 
stagedComputation :: [a->a] -> a -> [a]
stagedComputation fs x = tail $ scanl (flip id) x fs
 
stagedComputation' :: [a->a] -> a -> [a]
stagedComputation' = flip $ scanl (flip id) >>> (tail .)
 
main = do
    print $ stagedComputation   [(+1), (*2), subtract 3] 4
    print $ stagedComputation'  [(+1), (*2), subtract 3] 4