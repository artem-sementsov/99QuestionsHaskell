import Data.Map as Map hiding (map)

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