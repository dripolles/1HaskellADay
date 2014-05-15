data Gabuzomeu a b = Gabu a | Zomeu b

shadok :: (a -> c) -> (b -> d) -> Gabuzomeu a b -> Gabuzomeu c d
shadok f _ (Gabu x) = Gabu $ f x
shadok _ f (Zomeu x) = Zomeu $ f x

newtype G b a = G {first :: Gabuzomeu a b}

instance Functor (G b) where
  --fmap :: (a -> c) -> G b a -> G b c
  fmap f = G . (shadok f id ) . first

newtype Z a b = Z {second :: Gabuzomeu a b}

instance Functor (Z a) where
  -- fmap :: (b -> c) -> Z a b -> Z a c
  fmap f = Z . (shadok id f) . second

main = do
    print $ "OK"
