-- using this for left folds
import Data.List (foldl', group, partition, permutations, sort, sortBy, subsequences)
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
  | a < b = [a .. b]
  | otherwise = [a, (pred a) .. b]

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
  return $ perms !! head (randomRs (0, pred . factorial $ length list) gen)
  where
    perms = permutations list
    factorial n = product [1 .. n]

-- Generate the combinations of K distinct objects chosen from the N elements of a list'
combinations :: Int -> [a] -> [[a]]
combinations n list = [a | a <- subsequences list, length a == n]

-- Group the elements of a set into disjoint subsets.
-- HMMM, yeah no

-- sort by length
lsort :: [[a]] -> [[a]]
lsort [] = []
lsort (x : xs) = lsort lt ++ [x] ++ lsort gt
  where
    (lt, gt) = partition (\lambda -> length lambda < length x) xs

-- sort by length frequency
lfsort :: [[a]] -> [[a]]
lfsort list = map snd . sortBy byTupleFst $ tuplify list
  where
    byTupleFst (a, _) (b, _)
      | a < b = LT
      | a > b = GT
      | otherwise = EQ
    tuplify = map (\a -> (occurences a, a))
      where
        occurences x = length $ filter (\b -> length b == length x) list

-- Determine whether a given integer number is prime.
isPrime :: Integer -> Bool
isPrime x
  | x < 0 = isPrime $ negate x
  | x <= 3 = x > 1
  | even x || x `rem` 3 == 0 = False
  | otherwise = recursiveCheck x 5
  where
    recursiveCheck x index
      | index ^ 2 > x = True
      | otherwise = not (x `rem` index == 0 || x `mod` (index + 2) == 0) && recursiveCheck x (index + 6)

-- Determine the greatest common divisor of two integer numbers
myGCD :: Integer -> Integer -> Integer
myGCD a 0 = abs a
myGCD a b = gcd b (a `mod` b)

-- Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.
coprime :: Integer -> Integer -> Bool
coprime a b = myGCD a b == 1

-- Calculate Euler's totient function phi(m).
totient :: Integer -> Int
totient x = (length . filter (`coprime` x)) [1 .. (pred x)]

-- Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order.
-- Thank you, haskell lazy eval
primeFactors :: Integer -> [Integer]
primeFactors num = sort $ factors [] primes num
  where
    primes = filter isPrime [2 ..]
    factors acc _ 1 = acc
    factors acc list@(x : xs) n
      | n `rem` x == 0 = factors (x : acc) list $ n `div` x
      | otherwise = factors acc xs n

-- Determine the prime factors of a given positive integer.
-- Construct a list containing the prime factors and their multiplicity.
primeFactorsMult :: Integer -> [(Integer, Integer)]
primeFactorsMult = map (\list@(x : xs) -> (x, fromIntegral . length $ list)) . group . primeFactors

-- Calculate Euler's totient function phi(m) (improved).
phiMproved :: Integer -> Integer
phiMproved num = foldr (\(p, m) xs -> (p - 1) * p ^ (m - 1) * xs) 1 $ primeFactorsMult num

-- Problem 38, solutions compared :P

-- Given a range of integers by its lower and upper limit, construct a
-- list of all prime numbers in that range.
primesR :: Integer -> Integer -> [Integer]
primesR a b = filter isPrime [a .. b]

-- Goldbach's conjecture.
goldbach :: Integer -> (Integer, Integer)
goldbach 2 = error "The number has to be greater than two!"
goldbach num =
  let primeList = dropWhile (\x -> not $ isPrime $ num - x) $ filter isPrime [2 .. num]
   in conjecture primeList
  where
    conjecture []
      | odd num = error "It has to be an even number for goldbach's conjecture to work!"
      | otherwise = error "Please contact a mathematician, Goldbach is wrong!"
    conjecture (x : xs) = (x, num - x)

-- Given a range of integers by its lower and upper limit, 
-- print a list of all even numbers and their Goldbach composition.
goldbachList :: Integer -> Integer -> [(Integer, Integer)]
goldbachList a b = map goldbach $ filter even [a..b]

-- In most cases, if an even number is written as the sum of two prime numbers, 
-- one of them is very small. Very rarely, the primes are both bigger than say 50. 
-- Try to find out how many such cases there are in the range 2..3000.
goldbachList' :: Integer -> Integer -> Integer -> [(Integer, Integer)]
goldbachList' a b inc = filter (\(a, b) -> a > inc && b > inc) $ map goldbach $ filter even [3..b]

