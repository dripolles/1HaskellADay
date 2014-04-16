-- | update update the nth element of a list
-- if the index is not a valid index, it leaves the list unchanged
--
-- Examples
--
-- >>> update (-2) 10 [0..4]
-- [0,1,2,3,4]
--
-- >>> update 6 10 [0..4]
-- [0,1,2,3,4]
--
-- >>> update 2 10 [0..4]
-- [0,1,10,3,4]
 
update :: Int -> a -> [a] -> [a]
update i x =
    let f a b = if i == a then x else b
    in zipWith f [0..]
 
-- Just because I can, no "if"
update' :: Int -> a -> [a] -> [a]
update' i x =
    map (snd . head) .
    map (filter fst) .
    zipWith (:) (zip (map (i ==) [0..]) (repeat x)) .
    map ((:[]) . (,) True)
 
 
 
main = do
    print $ update (-2) 10 [0..4]
    print $ update 6 10 [0..4]
    print $ update (2) 10 [0..4]