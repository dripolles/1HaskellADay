import Control.Applicative
import Data.Tuple
import Control.Monad

{- Yesterday, we appended swapped values:

   Example
   > appendSwapped [(1,2),(2,3)]
   [(1,2),(2,3),(2,1),(3,2)]
-}
appendSwapped :: [(a,a)] -> [(a,a)]
appendSwapped = (++) <*> map swap

{- Today, we'll interleave them:

   Example
   > appendSwapped [(1,2),(2,3)]
   [(1,2),(2,1),(2,3),(3,2)]
-}
interleaveSwapped :: [(a,a)] -> [(a,a)]
interleaveSwapped = ((:) <*> return . swap =<<)

main = do
    print $ interleaveSwapped [(1,2),(3,4)]
