import Data.Monoid

{- Here is a PartialFunction Newtype -}
newtype PartialFunction a b = PF {pf :: a -> Maybe b}

{- Propose an instance of Monoid For PartialFunction 
   (see the example below for implementations clues)
 -}
instance Monoid (PartialFunction a b) where
  mempty = PF (const Nothing)
  mappend (PF f) (PF g) = PF f' where
    f' x = case f x of
        x'@(Just _) -> x'
        Nothing -> g x

{- Use the monoid to compose two partial functions:
   * the 1st divide by 2 an even value <= 1000
   * the 2nd multiply by 2 a value <= 500

   >>> pfMonoidExample 501
   Nothing

   >>> pfMonoidExample  400
   Just 200

   >>> pfMonoidExample  201
   Just 402
-}

f1 :: Int -> Maybe Int
f1 x | (even x) && (x <= 1000) = Just $ x `div` 2
     | otherwise = Nothing

f2 :: Int -> Maybe Int
f2 x | x <= 500 = Just $ x * 2
     | otherwise = Nothing

pfMonoidExample :: Int -> Maybe Int
pfMonoidExample = let (PF pf) = (PF f1) `mappend` (PF f2) in pf

main = do
    print $ pfMonoidExample 501
    print $ pfMonoidExample 400
    print $ pfMonoidExample 201
