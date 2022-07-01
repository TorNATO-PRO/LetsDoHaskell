-- attempting Haskell's 99 problems

import Data.List (foldl', nub, sortBy, (\\))
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random (Random (randomRs), getStdGen)

-- returns the last element of a list
last' :: [a] -> Maybe a
last' [] = Nothing
last' [x] = Just x
last' (_ : xs) = last' xs

-- returns the last two elements of a list
lastTwo :: [a] -> Maybe (a, a)
lastTwo [] = Nothing
lastTwo [x] = Nothing
lastTwo [x, y] = Just (x, y)
lastTwo (_ : xs) = lastTwo xs

-- obtains the nth element of a list
nthElement :: [a] -> Int -> Maybe a
nthElement [] _ = Nothing
nthElement (x : _) 0 = Just x
nthElement (_ : xs) n
  | n < 0 = Nothing
  | otherwise = nthElement xs (n - 1)

-- determines the length of a list
length' :: [a] -> Int
length' = foldr (\_ xs -> 1 + xs) 0

-- reverses a list
rev :: [a] -> [a]
rev = foldl' (flip (:)) []

-- determines whether a list is a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome lst = all (uncurry (==)) $ zip lst (rev lst)

-- Nested List Data Structure
data NestedList a = Elem a | List [NestedList a]

-- flatten a list
flatten' :: NestedList a -> [a]
flatten' (Elem a) = [a]
flatten' (List []) = []
flatten' (List xs) = concatMap flatten' xs

-- eliminates consecutive duplicate elements
compress :: (Eq a) => [a] -> [a]
compress = foldr (\x xs -> x : dropWhile (== x) xs) []

-- packs consecutive duplicate elements into sublists
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x : xs) = (x : takeWhile (== x) xs) : pack (dropWhile (== x) xs)

-- perform length encoding of a list
encode :: Eq a => [a] -> [(Int, [a])]
encode = map (\a -> (length' a, a)) . pack

-- List Encoding Data Structure
data EncodingData a = Multiple Int a | Single a deriving (Show)

-- a modified encoding of a list
encodeModified :: Eq a => [a] -> [EncodingData a]
encodeModified = map encodeList . pack
  where
    encodeList [] = error "This empty list is not supposed to be here"
    encodeList [x] = Single x
    encodeList lst@(x : _) = Multiple (length' lst) x

-- decodes a list
decodeModified :: Eq a => [EncodingData a] -> [a]
decodeModified = concatMap duplicateElements
  where
    duplicateElements (Single x) = [x]
    duplicateElements (Multiple n x) = replicate n x

-- directly length encodes a list
encodeDirect :: (Eq a) => [a] -> [EncodingData a]
encodeDirect [] = []
encodeDirect lst@(x : xs) = obtainEncoding (countFirstOccurences lst x) x : encodeDirect (dropWhile (== x) xs)
  where
    obtainEncoding :: Int -> a -> EncodingData a
    obtainEncoding 1 x = Single x
    obtainEncoding n x = Multiple n x
    countFirstOccurences :: Eq a => [a] -> a -> Int
    countFirstOccurences [] _ = 0
    countFirstOccurences (x : xs) a
      | x /= a = 0
      | otherwise = 1 + countFirstOccurences xs a

-- duplicates the elements of a list
dupli :: [a] -> [a]
dupli = foldr (\x xs -> x : x : xs) []

-- replicates the elements of a list
repli :: [a] -> Int -> [a]
repli lst n = foldr (\x xs -> replicate n x ++ xs) [] lst

-- drops every n-th element of a list
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery lst n = take (n - 1) lst ++ dropEvery (drop n lst) n

-- splits a list
split :: [a] -> Int -> ([a], [a])
split lst n = (take n lst, drop n lst)

-- extract a slice from a list
slice :: [a] -> Int -> Int -> [a]
slice lst l r = take (r - l + 1) $ drop (l - 1) lst

-- rotates a list
rotate :: [a] -> Int -> [a]
rotate lst n = let k = n `mod` length lst in drop k lst ++ take k lst

-- remove the k-th element from the list
removeAt :: Int -> [a] -> Maybe (a, [a])
removeAt n lst
  | n <= 0 = Nothing
  | otherwise =
    let lstSuffix = drop (n - 1) lst
     in if null lstSuffix then Nothing else Just (head lstSuffix, take (n - 1) lst ++ tail lstSuffix)

-- inserts an element into a given position in a list
insertAt :: a -> [a] -> Int -> [a]
insertAt a lst n
  | n <= 0 || n > length lst = lst
  | otherwise = take (n - 1) lst ++ (a : drop (n - 1) lst)

-- create a list of integers in a given range
range :: Int -> Int -> [Int]
range a b = if b >= a then [a .. b] else reverse [b .. a]

-- randomly select elements from a list
rndSelect :: [a] -> Int -> IO [a]
rndSelect [] _ = return []
rndSelect lst n = do
  generator <- getStdGen
  let indices = take n $ randomRs (0, length lst - 1) generator
  return [lst !! i | i <- indices]

-- select n random numbers from the set 1 .. m
diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = do
  if n > m
    then error $ "Cannot obtain " ++ show n ++ " distinct numbers"
    else take n . nub . randomRs (1, m) <$> getStdGen

-- obtains a random permutation of a list
rndPermu :: [a] -> IO [a]
rndPermu lst = do
  nums <- take (length lst) . nub . randomRs (0, length lst - 1) <$> getStdGen
  return [lst !! n | n <- nums]

-- generates the combinations of k distinct object from the n elements of a list
combinations :: Int -> [a] -> [[a]]
combinations _ [] = []
combinations n lst@(x : xs)
  | n <= 0 = []
  | n == 1 = [[a] | a <- lst]
  | otherwise = [x : a | a <- combinations (n - 1) xs] ++ combinations n xs

-- groups the elements of a set into disjoint subsets
group' :: (Eq a) => [Int] -> [a] -> [[[a]]]
group' [] _ = []
group' [_] lst = [[lst]]
group' (d : ds) lst = [c : g | c <- combinations d lst, g <- group' ds (lst \\ c)]

-- sort a list according to the length of sublists
lsort :: Foldable t => [t a] -> [t a]
lsort = map snd . sortBy (\(a, _) (b, _) -> compare a b) . map (\x -> (length x, x))

-- length frequency sort
lfsort :: (Foldable t1, Foldable t2) => t1 (t2 a) -> [t2 a]
lfsort lst = concat . lsort . map snd . Map.toList $ foldr ((\(l, e) xs -> Map.insert l (e : Map.findWithDefault [] l xs) xs) . (\x -> (length x, x))) Map.empty lst

-- determine whether a given integer is prime
isPrime :: Integral a => a -> Bool
isPrime x
  | x < 2 = False
  | otherwise = null [n | n <- [2 .. (floor $ sqrt $ fromIntegral x)], x `rem` n == 0]

-- determines the greatest common divisor of two positive integer numbers
myGCD :: Integral t => t -> t -> t
myGCD a 0 = abs a
myGCD a b = myGCD b (a `mod` b)

-- determines whether two positive integer numbers are coprime.
coprime :: Integral t => t -> t -> Bool
coprime a b = myGCD a b == 1

-- euler's totient function phi(m)
totient :: (Show a, Integral a) => a -> Int
totient 1 = 1
totient m
  | m < 1 = error $ "Cannot find the totient of (" ++ show m ++ ") as it is less than 1"
  | otherwise = length $ filter (coprime m) [1 .. (m - 1)]

-- a list of prime numbers from a up to b
primes :: Integral a => a -> a -> [a]
primes a b = [c | c <- [a .. b], isPrime c]

-- prime factors of a number
primeFactors :: Integral a => a -> [a]
primeFactors n
  | n < 2 = error "Can only find prime factors of positive integers greater than 2"
  | isPrime n = [n]
  | otherwise = primeFactors' [] n $ primes 2 n
  where
    primeFactors' acc 1 _ = reverse acc
    primeFactors' acc _ [] = error "Unable to find prime factors"
    primeFactors' acc k lst@(x : xs)
      | k `rem` x == 0 = let newNumber = k `div` x in primeFactors' (x : acc) newNumber $ primes x newNumber
      | otherwise = primeFactors' acc k xs

-- prime factors and their multiplicity
primeFactorsMult :: (Num b, Integral a) => a -> [(a, b)]
primeFactorsMult n = let (a : as) = primeFactors n in primeFactorsCombined a 1 as
  where
    primeFactorsCombined d c [] = [(d, c)]
    primeFactorsCombined d c (x : xs)
      | d == x = primeFactorsCombined x (c + 1) xs
      | otherwise = (d, c) : primeFactorsCombined x 1 xs

-- euler's improved totient function
phi :: (Integral a, Show a) => a -> a
phi 1 = 1
phi m
 | m < 1 = error $ "Cannot find the totient of (" ++ show m ++ ") as it is less than 1"
 | otherwise = product $ map (\(p, m) -> (p - 1) * p ^ (m - 1)) $ primeFactorsMult m

-- goldbach's conjecture
goldbach :: Integral a => a -> (a, a)
goldbach n
 | n <= 2 || odd n = error "Goldbach's conjecture only works on positive even numbers greater than 2"
 | otherwise = head [(x, y)| x <- primes 2 (n - 2), y <- primes 2 (n - 2), x + y == n]

-- a list of even numbers in the goldbach list
goldbachList :: Integral a => a -> a -> [(a, a)]
goldbachList a b = [goldbach n | n <- [a..b], even n && n > 2]

-- all of the numbers who's primes are greater than a certain number
goldbachList' :: Integral a => a -> a -> a -> [(a, a)]
goldbachList' a b n = [(c, d) | (c, d) <- goldbachList a b, c > n && d > n]

-- logic definitions 
and' :: Bool -> Bool -> Bool
and' = (&&)

or' :: Bool -> Bool -> Bool
or' = (||)

nand' :: Bool -> Bool -> Bool
nand' x y = not $ and' x y

nor' :: Bool -> Bool -> Bool
nor' x y = not $ or' x y

xor' :: Bool -> Bool -> Bool
xor' x y = and' (or' x y) (not (and' x y))

impl' :: Bool -> Bool -> Bool
impl' x y = not (and' x $ not y)

equiv' :: Bool -> Bool -> Bool
equiv' x y = or' (and' x y) (and' (not x) (not y))

-- make them infix with a precedence
infixl 1 `or'`
infixl 2 `xor'`
infixl 3 `and'`
infixl 4 `equiv'`
infixl 1 `nor'`
infixl 3 `nand'`
infixl 4 `impl'`

-- table prints the truth table of a given logical expression of two variables
table :: (Bool -> Bool -> Bool) -> [[Bool]]
table a = [[c, d, a c d]| c <- [True, False], d <- [True, False]]

tablen :: (Num t, Ord t) => t -> ([Bool] -> Bool) -> [[Bool]]
tablen n a = map ((\x -> x ++ [a x]) . reverse) (tableOfBools n)
 where
  tableOfBools 1 = [[a] | a <- [True, False]]
  tableOfBools n
   | n < 1 = error "n must be greater than 0"
   | otherwise = [b : a | a <- tableOfBools (n - 1), b <- [True, False]]

gray :: (Ord a, Num a) => a -> [[Char]]
gray 1 = ["0", "1"]
gray n
 | n < 1 = error "Can only find gray codes of numbers greater than 0"
 | otherwise = ['0' : b | b <- gray (n - 1)] ++ ['1' : b | b <- gray (n - 1)]

