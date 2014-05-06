import Data.Tree

zipTree :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
zipTree f (Node x forestx) (Node y foresty) =
    let forest = zipWith (zipTree f) forestx foresty
    in Node (f x y) forest 

main = do
    print $ zipTree (+) (Node 1 [Node 2 [], Node 3[]]) (Node 1 [Node 2 []])
