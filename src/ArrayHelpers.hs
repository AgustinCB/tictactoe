module ArrayHelpers (replace, replace2D, mapInd, pick) where

import System.Random (randomRIO)

replace :: (a -> a) -> Int -> [a] -> [a]
replace f 0 (x:xs) = (f x):xs
replace f i (x:xs) = x : replace f (i-1) xs
replace f i [] = []

replace2D :: (a -> a) -> Int -> Int -> [[a]] -> [[a]]
replace2D f x y = replace (replace f y) x

mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]

pick :: [a] -> IO a
pick xs = fmap (xs !!) $ randomRIO (0, length xs - 1)
