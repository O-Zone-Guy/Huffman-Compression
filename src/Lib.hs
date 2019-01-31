module Lib where

{- An algebraec data structure used to simulate the key for a Huffman Compression algorithm
 - First item in structure is the most common
 - Branch x1 (Branch x2 ... (Leaf xn))
 - Where x1 is the most common item in a list/input and xn is the least common item
 -}
data Comp a = Leaf a
            | Branch a (Comp a)

test :: IO ()
test = putStrLn "Hello World"

{- returns a list of tuples containing the frequency of an element and the element
 - uses aux function unique to isolate all elements
 - then uses map to create the list of tuples where the second argument is the element and the first argument is the frequency
 - the frequency is the length of the list evlauted using filter(==u) xs where u is the unique element and xs is the original list
 -}
frequency :: Eq a => [a] -> [(Int, a)]
frequency xs = let
 unique xs [] = xs
 unique xs (y:ys) | y `elem` xs = unique xs ys
                  | otherwise   = unique (y:xs) ys
 count xs = map (\u-> (length (filter (==u) xs),u))
 in count xs (unique [] xs)

{- orders the frequency tuples based on the frequency, fst elem, and using sortInsert func
 - recursivly splits the list in half untill there is only one element left
 - takes each half pairs and passes them to sortInsert where they are sorted using the sortInsert algorithm
 -}
order :: [(Int, a)] -> [(Int, a)]
order [] = []
order [x] = [x]
order xs = let
 h1 = take (length xs `div` 2) xs
 h2 = drop (length xs `div` 2) xs
 in sortInsert (order h1) (order h2)

{- Uses sortInsert algorithm to sort and unsosrted list of type [(Int, a)] into a sorted list
 -}
sortInsert :: [(Int, a)] -> [(Int, a)] -> [(Int, a)]
sortInsert (x:xs) (y:ys) | fst x >= fst y = sortInsert xs (x:y:ys)
                         | otherwise      = y:sortInsert (x:xs) ys
sortInsert [] ys = ys
sortInsert xs [] = xs

{- takes a sorted list of type [(Int, a)] and generates a Comp a data to compress list used to generate the sorted list
 -}
sortedToHuff :: [(Int, a)] -> Comp a
sortedToHuff [] = error "Empty list"
sortedToHuff [x] = Leaf $ snd x
sortedToHuff (x:xs) = Branch (snd x) (sortedToHuff xs)


{- takes a list of type [a] and generates a tuple of the compressed list and the Comp a key used to compress it, (Comp a, [Int]).
 - creates the Comp a by using sortedToHuff, order and frequency in that order
 - uses aux function comp to compress the list using the generated Comp a
 - - comp takes three inputs, the list, x:xs, an Int, n, and the genereated Comp a, h.
 - - n starts at 0 and the function goes through h and compares the first element of the list
 - - if x == the stored elemtent in the head of h, the top branch, then return n
 - - otherwise go to the next Branch in h and n+=1
 - - returns an error if there wsa no match
 - - the error should never occur, but just a precation to end the loop
 -}
compressList :: Eq a => [a] -> (Comp a,[Int])
compressList xs = let
 -- comp :: Eq a => [a] -> Int -> Comp a -> [Int]
 comp [] _ _ = []
 comp (x:xs) n (Leaf a)     | a == x    = n : comp xs 0 h
                            | otherwise = error "Wrong Comp data"
 comp (x:xs) n (Branch a c) | x == a    = n: comp xs 0 h
                            | otherwise = comp (x:xs) (n+1) c
 h = sortedToHuff $ order $ frequency xs
 in (h, comp xs 0 h)

{- takes a tuple, (Comp a, [int]) where [Int] is the compressed list and returns the uncompressed list
 - uses aux funciton uncomp that takes an Int, n, and an h, Comp a.
 - - goes though h untill n == 0, for each level n-=1
 - - if it reaches the end of h and n /= 0 then it returns an error
 - - these errors shouldn't happen, but they are just a precaution in case something ever goes wrong and to end the loop
 -}

uncompressList :: (Comp a, [Int]) -> [a]
uncompressList (_,[]) = []
uncompressList (h, n:ns) = let
 uncomp n (Leaf x)       | n == 0    = x
                         | otherwise = error "Wrong Comp data"
 uncomp n (Branch x c)   | n == 0    = x
                         | otherwise = uncomp (n-1) c
 in uncomp n h : uncompressList (h,ns)
