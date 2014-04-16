-- http://codepad.org/QollsjCw
import Control.Monad
import Data.Function
import Data.List
import Test.QuickCheck
 
-- $setup
-- >>> import Control.Applicative ((<*>))
-- >>> import Data.List (isInfixOf)
-- >>> import Test.QuickCheck
 
-- Level: Easy
-- Pointfree: yes
 
 
-- | mostRepeatedElem
-- Returns the element with the longest (consecutive) repetition and the
-- repetition number
-- If there are tie, the last most repeated element is returned
-- It returns error on empty string
-- 
-- Examples:
--
-- >>> mostRepeatedElem "hello world!"
-- ('l',2)
--
-- >>> mostRepeatedElem [1,1,2,2]
-- (2,2)
--
-- prop> (flip isInfixOf <*> uncurry (flip replicate) . mostRepeatedElem) . getNonEmpty
 
mostRepeatedElem :: Eq a => [a] -> (a,Int)
mostRepeatedElem =
    maximumBy (compare `on` snd) .
    map (liftM2 (,) head length) . -- also map (head &&& length)
    group
 
main = do
    print $ mostRepeatedElem "Hello!!!1one"