module Main where
{- You have a list of rooms, each rooms has two doors and contain one element.
  To enter a room, the first door must be opened. To leave a room, the second door
  must be open. It means that if you want to move from one room to the next one,
  both the second door of the first room and the first door of the second room.
-}
data Room a = Room (Bool, Bool) a

{- | Given a list of rooms, we want to go as far as we can and collect the elements
  in he room. This is the purpose of the function `collect`.

  Examples:

  >>> collect [Room (True, True) 1, Room (True, False) 2, Room (True, False) 3]
  [1,2]

  >>> collect [Room (False, True) 1, Room (True, False) 2, Room (True, False) 3]
  []

  >>> collect [Room (True, True) 'a', Room (False, True) 'b', Room (True, True) 'c']
  ['a']
-}

collect :: [Room a] -> [a]
collect = foldr go []  where
    go (Room (l,r) x) xs
        | l = x : if r then xs else []
        | otherwise = []


main = do
    print $ collect [Room (True, True) 1, Room (True, False) 2, Room (True, False) 3]
    print $ collect [Room (False, True) 1, Room (True, False) 2, Room (True, False) 3]
    print $ collect [Room (True, True) 'a', Room (False, True) 'b', Room (True, True) 'c']
    let inf = (Room (True, True) 2):inf
    print $ collect $ (Room (True, False) 1):inf
