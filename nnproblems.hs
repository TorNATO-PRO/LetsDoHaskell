-- using this for left folds
-- using this for left folds
import Data.List (foldl', permutations)
import System.Random

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
pack [] = []
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
encode = map (\(x : xs) -> (myLength (x : xs), x)) . pack

-- define a new data type to do the next questions
data Encoded a = Multiple Int a | Single a deriving (Show)

-- Modified run-length encoding.
-- Modify the result of problem 10 in such a way that if an element has
-- no duplicates it is simply copied into the result list. Only elements with
-- duplicates are transferred as (N E) lists.
encodeModified :: Eq b => [b] -> [Encoded b]
encodeModified = map (\list@(x : xs) -> encode' x $ length list) . pack
  where
    encode' x 1 = Single x
    encode' x n = Multiple n x

-- Decode a run-length encoded list
-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
decodeModified :: [Encoded a] -> [a]
decodeModified = concatMap decode'
  where
    decode' (Multiple n a) = replicate n a
    decode' (Single a) = [a]

-- Run-length encoding of a list (direct solution).
-- Implement the so-called run-length encoding data compression method directly.
-- I.e. don't explicitly create the sublists containing the duplicates, as in problem 9,
-- but only count them. As in problem P11, simplify the result list by
-- replacing the singleton lists (1 X) by X.
encodeDirect :: Eq a => [a] -> [Encoded a]
encodeDirect [] = []
encodeDirect (x : xs) = encodeDirect' xs x 1
  where
    encodeDirect' [] curr 1 = [Single curr]
    encodeDirect' [] curr n = [Multiple n curr]
    encodeDirect' (d : ds) curr total
      | d == curr = encodeDirect' ds curr (succ total)
      | otherwise =
        if total == 1
          then Single curr : encodeDirect' ds d 1
          else Multiple total curr : encodeDirect' ds d 1

-- Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli = foldr (\x xs -> x : x : xs) []

-- Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli list k = repli' list k
  where
    repli' [] _ = []
    repli' (x : xs) 1 = x : repli' xs k
    repli' (x : xs) n = x : repli' (x : xs) (pred n)

-- Drop every N'th element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery list k = dropEvery' list k
  where
    dropEvery' [] _ = []
    dropEvery' (x : xs) 1 = dropEvery' xs k
    dropEvery' (x : xs) n = x : dropEvery' xs (pred n)

-- Split a list into two parts; the length of the first part is given.
-- Yeah im lazy, your point?
split :: [a] -> Int -> ([a], [a])
split list k = splitAt k list

-- Extract a slice from a list.
-- Slices a list containing the elements between the i'th and k'th element
-- of the original list (both limits included). Start counting the elements
-- with 1.
slice :: [a] -> Int -> Int -> [a]
slice list i k = slice' list i k 1
  where
    slice' [] _ _ _ = []
    slice' (x : xs) i k n
      | n >= i && n <= k = x : slice' xs i k (succ n)
      | n > k = []
      | otherwise = slice' xs i k $ succ n

-- Rotate a list N places to the left.
-- Linked lists and their efficiency lmao (I'm being sarcastic)
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate list 0 = list
rotate list@(x : xs) n
  | n > 0 = rotate (xs ++ [x]) (pred n)
  | otherwise = rotate list $ length list + n

-- remove from the list a given element
removeAt :: (Num t, Ord t, Enum t) => t -> [a] -> (a, [a])
removeAt n list = removeAt' n list []
  where
    removeAt' _ [] acc = error "Index out of range"
    removeAt' 1 (x : xs) acc = (x, reverse acc ++ xs)
    removeAt' n (x : xs) acc = removeAt' (pred n) xs (x : acc)

-- insert an element at a given position into a list
insertAt :: a -> [a] -> Int -> [a]
insertAt _ [] _ = error "Index out of range"
insertAt elem (x : xs) 1 = elem : x : xs
insertAt elem (x : xs) index = x : insertAt elem xs (pred index)

-- Create a list containing all integers within a given range.
range :: Integer -> Integer -> [Integer]
range a b
 | a < b = [a..b]
 | otherwise = [a,(pred a)..b]

-- Extract a given number of randomly seleced elements
rndSelect1 :: [a] -> Int -> IO [a]
rndSelect1 list k = do
  gen <- getStdGen
  let nums = take k $ randomRs (0, pred . length $ list) gen
  return $ map (list !!) nums

-- Lotto: Draw N different random numbers from the set 1..M.
rndSelect2 :: Int -> Int -> IO [Int]
rndSelect2 n m = do
  gen <- getStdGen
  let nums = take n $ randomRs (1, m) gen
  return nums

-- Generate a random permutation of the elements of a list.
-- Well isn't this efficient :P
rndPermu :: [a] -> IO [a]
rndPermu list = do
  gen <- getStdGen
  let perms = permutations list
  return $ perms !! head (randomRs (0, pred . length $ perms) gen)
