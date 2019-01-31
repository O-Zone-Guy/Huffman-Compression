module Lib where

import Numeric
import Data.Char

data Comp a = Leaf a
            | Branch a (Comp a)

test :: IO ()
test = putStrLn "Hello World"

{- returns a list of tuples containing the frequency of an element and the element -}
frequency :: Eq a => [a] -> [(Int, a)]
frequency xs = let
 unique xs [] = xs
 unique xs (y:ys) | y `elem` xs = unique xs ys
                  | otherwise   = unique (y:xs) ys
 count xs = map (\u-> (length (filter (==u) xs),u))
 in count xs (unique [] xs)

order :: [(Int, a)] -> [(Int, a)]
order [] = []
order [x] = [x]
order xs = let
 h1 = take (length xs `div` 2) xs
 h2 = drop (length xs `div` 2) xs
 in sortInsert (order h1) (order h2)

sortInsert :: [(Int, a)] -> [(Int, a)] -> [(Int, a)]
sortInsert (x:xs) (y:ys) | fst x >= fst y = sortInsert xs (x:y:ys)
                         | otherwise      = y:sortInsert (x:xs) ys
sortInsert [] ys = ys
sortInsert xs [] = xs

sortedToHuff :: [(Int, a)] -> Comp a
sortedToHuff [] = error "Empty list"
sortedToHuff [x] = Leaf $ snd x
sortedToHuff (x:xs) = Branch (snd x) (sortedToHuff xs)

compressList :: Eq a => [a] -> [Int]
compressList xs = let
 -- comp :: Eq a => [a] -> Int -> Comp a -> [Int]
 comp [] _ _ = []
 comp (x:xs) n (Leaf a)     | a == x    = n : comp xs 0 h
                            | otherwise = error "Wrong Comp data"
 comp (x:xs) n (Branch a c) | x == a    = n: comp xs 0 h
                            | otherwise = comp (x:xs) (n+1) h
 h = sortedToHuff $ order $ frequency xs
 in comp xs 0 h
