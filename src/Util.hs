module Util where

replace :: Int -> a -> [a] -> [a]
replace i element (first : others)
    | i < 0 || i >= length others + 1 = error "Invalid index"
    | i == 0 = element : others
    | otherwise = first : replace (i - 1) element others