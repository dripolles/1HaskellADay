import Prelude hiding (head, tail)
import Control.Applicative

-- Define a data type for non empty list.
data NonEmptyList a = L a [a] deriving Show

-- head of a NonEmptyList
head :: NonEmptyList a -> a
head (L x  _) = x

-- tail of a NonEmptyList
tail :: NonEmptyList a -> [a]
tail (L _ xs) = xs

-- builder from list
fromList :: [a] -> Maybe (NonEmptyList a)
fromList [] = Nothing
fromList (x:xs) = Just $ L x xs

-- From NonEmptyList to List
toList :: NonEmptyList a -> [a]
toList = liftA2 (:) head tail

main = do
   print $ (fromList [] :: Maybe (NonEmptyList Int))
   print $ fromList [1]
   print $ fromList [1..3]
