data Gabuzomeu a b = Gabu a | Zomeu b deriving Show

shadok :: (a -> c) -> (b -> d) -> Gabuzomeu a b -> Gabuzomeu c d
shadok f _ (Gabu x) = Gabu $ f x
shadok _ f (Zomeu x) = Zomeu $ f x

newtype G b a = G {first :: Gabuzomeu a b} deriving Show

instance Functor (G b) where
  --fmap :: (a -> c) -> G b a -> G b c
  fmap f = G . (shadok f id ) . first

newtype Z a b = Z {second :: Gabuzomeu a b} deriving Show

instance Functor (Z a) where
  -- fmap :: (b -> c) -> Z a b -> Z a c
  fmap f = Z . (shadok id f) . second

main = do
    print $ fmap (+2) (G (Gabu 2)::G Char Int)
    print $ fmap (+2) (G (Zomeu 'a')::G Char Int)
    print $ fmap succ (Z (Gabu 2)::Z Int Char)
    print $ fmap succ (Z (Zomeu 'a')::Z Int Char)
