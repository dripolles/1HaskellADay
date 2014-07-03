import Data.Monoid

{- Here is a PartialFunction Newtype -}
newtype PartialFunction a b = PF {pf :: a -> Maybe b}

{- We saw yesterday that PartialFunction is a Monoid. It's also a Functor
   And this one is easy.
 -}

instance Functor (PartialFunction a) where
    fmap = undefined

main = do
    print "OK"
