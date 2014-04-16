import Control.Applicative
import Control.Arrow
 
foo :: (a ->  b) -> [a] -> [(a,b)]
foo f = map ((,) <$> id <*> f)
 
foo' :: (a ->  b) -> [a] -> [(a,b)]
foo' = map . (id &&&)
 
main = do
    print $ foo (>1) [0..4]
    print $ foo' (>1) [0..4]