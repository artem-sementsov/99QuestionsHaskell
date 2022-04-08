import Data.Map as Map hiding (drop, map, split, take)

-- 1 (*) Find the last element of a list.
myLast :: [a] -> a
myLast = head . reverse

myLast' [] = error "list is empty"
myLast' [v] = v
myLast' (x : xs) = myLast' xs

--2 (*) Find the last but one element of a list.
myButLast [] = error "list is empty"
myButLast [v] = error "list has only one element"
myButLast [a, b] = a
myButLast (x : xs) = myButLast xs

-- 3 (*) Find the K'th element of a list. The first element in the list is number 1.
elementAt :: [a] -> Int -> a
elementAt [] _ = error ""
elementAt (x : xs) 1 = x
elementAt (x : xs) n = elementAt xs (n -1)

-- 4 (*) Find the number of elements of a list.
myLength :: [a] -> Int
myLength list =
  let myLength' [] len = len
      myLength' (x : xs) len = myLength' xs (len + 1)
   in myLength' list 0

-- 5 (*) Reverse a list.
myreverse :: [a] -> [a]
myreverse list =
  let shift [] new = new
      shift (x : xs) new = x : new
   in shift list []

-- 6 (*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
myPalindrome :: (Eq a) => [a] -> Bool
myPalindrome list = reverse list == list

-- 7 (**) Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List xs) = concatMap flatten xs

--8 (**) Eliminate consecutive duplicates of list elements.
--If a list contains repeated elements they should be replaced with a single copy of the element.
--The order of the elements should not be changed.

compress :: (Eq a) => [a] -> [a]
compress list =
  let compress' new [] = reverse new
      compress' new (x : xs) =
        if x `elem` new
          then compress' new xs
          else compress' (x : new) xs
   in compress' [] list

-- 9 (**) Pack consecutive duplicates of list elements into sublists.
--If a list contains repeated elements they should be placed in separate sublists.
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack list =
  let pack' new [] = reverse new
      pack' [] (y : ys) = pack' [[y]] ys
      pack' (x : xs) (y : ys) =
        if y `elem` x
          then pack' ((y : x) : xs) ys
          else pack' ([y] : x : xs) ys
   in pack' [] list

-- 10 (*) Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data
-- compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates
-- of the element E.

encode :: (Eq a) => [a] -> [(Int, a)]
encode list = map (\x -> (length x, head x)) (pack list)

--11 (*) Modified run-length encoding. Modify the result of problem 10 in such a way that if an element has no duplicates
--it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

encodeModified :: [Char] -> [ZipLetter]
encodeModified list = map (\(count, c) -> if count == 1 then Single c else Multiple count c) (encode list)

data ZipLetter = Single Char | Multiple Int Char

instance Show ZipLetter where
  show (Single c) = "Single " ++ show c
  show (Multiple count c) = "Multiple " ++ show count ++ " " ++ show c

zipLetterToString :: ZipLetter -> String
zipLetterToString (Single c) = c : []
zipLetterToString (Multiple n c) = replicate n c

-- 12 (**) Decode a run-length encoded list.
--Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.

decodeModified :: [ZipLetter] -> [Char]
decodeModified arr = concat $ map zipLetterToString arr

-- 13 (**) Run-length encoding of a list (direct solution).
-- Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing
-- the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton
-- lists (1 X) by X.
encodeDirect :: [Char] -> [ZipLetter]
encodeDirect str =
  let encodeDirect' new [] = reverse new
      encodeDirect' [] (y : ys) = encodeDirect' [(y, 1)] ys
      encodeDirect' ((c, count) : xs) (y : ys) = if c == y then encodeDirect' ((c, count + 1) : xs) ys else encodeDirect' ((y, 1) : (c, count) : xs) ys
   in map letterFromPair $ encodeDirect' [] str

letterFromPair :: (Char, Int) -> ZipLetter
letterFromPair (c, 1) = Single c
letterFromPair (c, count) = Multiple count c

-- 14 (*) Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli [] = []
dupli (x : xs) = x : x : dupli xs

-- 15 (**) Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli [] n = []
repli (x : xs) n = replicate n x ++ repli xs n

-- 16 (**) Drop every N'th element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery list n =
  let dropEvery' [] n counter = []
      dropEvery' (x : xs) n 1 = dropEvery' xs n n
      dropEvery' (x : xs) n counter = x : dropEvery' xs n (counter -1)
   in dropEvery' list n n

-- 17 (*) Split a list into two parts; the length of the first part is given.
split :: [a] -> Int -> ([a], [a])
split list n = (take n list, drop n list)

-- 18 (**) Extract a slice from a list. Given two indices, i and k, the slice is the list containing the elements between
--the i'th and k'th element of the original list (both limits included). Start counting the elements with 1.
slice :: [a] -> Int -> Int -> [a]
slice list start end = take (end - start + 1) $ drop (start -1) list

--19 (**) Rotate a list N places to the left.
-- Hint: Use the predefined functions length and (++).
rotate :: [a] -> Int -> [a]
rotate list n = rotate' list $ n `mod` length list
  where
    rotate' list n = drop n list ++ take n list

--20 (*) Remove the K'th element from a list.
removeAt :: Int -> [a] -> (a, [a])
removeAt n list = ((!!) list (n -1), take (n -1) list ++ drop n list)

main :: IO ()
main = do
  print $ myLast [1, 2, 3]
  print $ myLast' [1, 2, 3]
  print $ myLast' [3]

  print $ myButLast [1, 2, 3]
  print $ myButLast [2, 3]

  print $ elementAt [1, 2, 3] 2

  print $ myLength "Hello, world!"

  print $ flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])

  print $ compress "aaaabccaadeeee"

  print $ pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']

  print $ encode "aaaabccaadeeee"

  print $ encodeModified "aaaabccaadeeee"

  print $ decodeModified [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']

  print $ encodeDirect "aaaabccaadeeee"

  print $ dupli [1, 2, 3]

  print $ repli "abc" 3

  print $ dropEvery "abcdefghik" 3

  print $ split "abcdefghik" 3

  print $ slice ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'k'] 3 7

  print $ rotate ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'] 3
  print $ rotate ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'] (-2)

  print $ removeAt 2 "abcd"