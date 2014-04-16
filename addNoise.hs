-- http://codepad.org/QomzEZSD
 
import System.Random
import Control.Arrow
 
addNoise :: (Num a, Random a) => a -> [a] -> StdGen -> ([a], StdGen)
addNoise x l gen = let
    range = ((id &&& negate) . abs) x
    noised = zipWith (+) l (randomRs range gen)
    in (noised, fst $ split gen)
 
-- It can be done in a line, but it's uglier
addNoise' x l gen =
    (zipWith (+) l (randomRs (((id &&& negate) . abs) x) gen), fst $ split gen)
 
main = do
    gen <- getStdGen
    let (l, gen') = addNoise 5 [1,1,1,1,1,1] gen :: ([Int], StdGen)
    print (l, gen')