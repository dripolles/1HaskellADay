{-# LANGUAGE BangPatterns #-}
import Data.Map (Map, empty, insertWith)
import Data.List (foldl')

{-| Count character occurrences in a string

    Examples:
    >>> lookup 'l' $ countOccurrences "hello"
    Just 2
    >>> lookup 'n' $ countOccurrences "hello"
    Nothing
-}
countOccurrences :: String -> Map Char Int
countOccurrences = foldl' go empty where
    go !acc !x = insertWith (+) x 1 acc

main = do
    let s = concat $ repeat "abc"
    print $ countOccurrences (take 9999999 s)
