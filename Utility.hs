{- |
Module      :  Utility
Description :  <Utility module holding various useful general functions>
Copyright   :  (c) Hunter A. Vallejos
License     :  N/A

Maintainer  :  <hunterandresvallejos@gmail.com>
Stability   :  experimental
Portability :  portable

<Utility module holding various useful general functions>
-}
module Utility where



infix 2 `xor`
-- | Exclusive OR statement
xor :: Bool -> Bool -> Bool
xor b1 b2 = (b1 && (not b2)) || ((not b1) && b2)

-- | Count elements in [a] that return True for (a -> Bool)
countSuchThat :: (a -> Bool) -> [a] -> Int
countSuchThat f x = length (filter f x)

-- | Compose a function with itself a given amount of times
doKTimes :: (a -> a) -> Int -> (a -> a)
doKTimes f k
    | k == 0 = error "can't do zero times!"
    | k == 1 = f
    | otherwise = f . (doKTimes f (k-1))


-- | Slides a selection window along a given list, with given window size
slideWindow :: [a] -> Int -> [[a]]
slideWindow list window_size = let
    n = length list
    in
    if window_size > n 
        then error ("Cannot slideWindow, window_size larger than list size!")
    else [[list !! index | index <- [start_index .. start_index+window_size-1]] | start_index <- [0.. n-window_size]]

-- | Enumerates a list and send [a, b, c, ...] to [(1, a), (2, b), (3, c), ...]
enumerate :: [a] -> [(Int, a)]
enumerate list = zip [0.. (length list)-1] list

-- | Gets the elements of a list whose indexes are in the interval [i1, i2]
getRange :: [a] -> Int -> Int -> [a]
getRange list i1 i2 = [list !! i | i <- [i1 .. i2]]