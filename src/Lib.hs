module Lib where

import Numeric
import Data.Char

data Comp a = Leaf a
            | Branch a (Comp a)

test :: IO ()
test = putStrLn "Hello World"

-- returns a list of tuples containing the frequency of an element and the element
frequency :: Eq a => [a] -> [(Int, a)]
frequency xs = let
 unique xs [] = xs
 unique xs (y:ys) | y `elem` xs = unique xs ys
                  | otherwise   = unique (y:xs) ys
 count xs [] = []
 count xs (u:us) = (length $ filter (==u) xs,u):count xs us
 in count xs (unique [] xs)
