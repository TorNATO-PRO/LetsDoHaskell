-- using this for left folds
import Data.List (foldl')

-- Find the last element of a list.
-- In regular haskell: myLast = last :P
-- Guess I have to do this with recursion and
-- not higher order functions huh?
myLast :: [p] -> p
myLast [] = error "The list is empty!"
myLast [x] = x
myLast (x : xs) = myLast xs

-- Find the last but one element of a list.
myButLast :: [c] -> c
myButLast = myLast . init

-- Find the K'th element of a list. The first
-- element in the list is number 1
-- The cheating way to do this would be
-- elementAt q x = q !! (pred x)
elementAt :: Integral t => [p] -> t -> p
elementAt [] _ = error "Empty list"
elementAt (x : xs) 1 = x
elementAt (x : xs) n = elementAt xs $ pred n

-- Find the number of elements of a list
-- Lets get that TCO and currying!!!
myLength :: [a] -> Integer
myLength = myLength' 0
  where
    myLength' n [] = n
    myLength' n (x : xs) = myLength' (succ n) xs

-- Reverse a list :P
-- Kinda like reversing a linkedlist for
-- an interview, but we are doing it the Chad
-- way. And no, I am not just going to do
-- myReverse = reverse :()
myReverse :: [a] -> [a]
myReverse = foldl' (flip (:)) []

-- Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
-- Be enlightened :P
isPalindrome :: Eq a => [a] -> Bool
isPalindrome list = foldr (\(a, b) xs -> a == b && xs) True $ zip list $ myReverse list

data NestedList a = Elem a | List [NestedList a]

-- Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem a) = [a]
-- concatMap flatten a
flatten (List a) = flatten' a
  where
    flatten' [] = []
    flatten' (x : xs) = flatten x ++ flatten' xs

-- Eliminate consecutive duplicates of list elements.
-- too lazy to do TCO!
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x : y : xs) = if x /= y then x : compress (y : xs) else compress (y : xs)

-- Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements they should be placed in separate sublists.
pack :: Eq a => [a] -> [[a]]
pack (x : xs) = pack' xs [x]
  where
    pack' [] acc = [acc]
    pack' (d : ds) acc
      | d == head acc = pack' ds (d : acc)
      | otherwise = acc : pack' ds [d]

-- Run-length encoding of a list. Use the result of problem P09 
-- to implement the so-called run-length encoding data compression method. 
-- Consecutive duplicates of elements are encoded as lists (N E) where N is the 
-- number of duplicates of the element E.
encode :: Eq b => [b] -> [(Integer, b)]
encode list = map (\(x:xs) -> (myLength (x:xs), x)) $ pack list


