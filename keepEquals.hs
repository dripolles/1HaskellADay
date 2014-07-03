import Data.Maybe
{-| Keep equal elements that are at the same position in both lists

  Examples:

  >>> keepEqual "hello" "world"
  "l"

  >>> keepEqual (repeat 1) [0..10]
  [1]

  >>> keepEqual [True, False, True] (repeat True)
  [True,True]
-}
keepEqual :: Eq a => [a] -> [a] -> [a]
keepEqual xs ys = catMaybes $ zipWith maybeEquals xs ys where
    maybeEquals x y = if x == y then Just x else Nothing
