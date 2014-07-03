module Iso1 where
import Test.QuickCheck

f :: Either a a -> (Bool, a)
f (Left x) = (False, x)
f (Right x) = (True, x)

g :: (Bool, a) -> Either a a
g (True, x) = Right x
g (False, x) = (Left x)

foo = (f . g)  (True,1)
