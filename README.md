# Nathan Does Haskell!

Attempting the [99 Haskell problems](https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems) (well, apparently there is actually only 88) to become a better [Haskell](https://www.haskell.org/) developer.

#### Some code that I am proud of:

```hs
isPalindrome :: Eq a => [a] -> Bool
isPalindrome list = foldr (\(a, b) xs -> a == b && xs) True $ zip list $ myReverse list
```

```hs
encode :: Eq b => [b] -> [(Integer, b)]
encode = map (\(x : xs) -> (myLength (x : xs), x)) . pack
```

```hs
lsort :: [[a]] -> [[a]]
lsort [] = []
lsort (x : xs) = lsort lt ++ [x] ++ lsort gt
  where
    (lt, gt) = partition (\lambda -> length lambda < length x) xs
```

```hs
combinations :: Int -> [a] -> [[a]]
combinations n list = [a | a <- subsequences list, length a == n]
```

```hs
myGCD :: Integer -> Integer -> Integer
myGCD a 0 = abs a
myGCD a b = gcd b (a `mod` b)
```

```hs
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
```

Enjoy, and make sure to try these out yourself! If looking at this hasn't convinced you to start trying to learn Haskell, here is perhaps a [better resource](https://wiki.haskell.org/Why_Haskell_matters). There is a reason behind the relatively recent functional programming fad, which is also something that [you should look into](http://www.cse.chalmers.se/~rjmh/Papers/whyfp.html). It is not just smug Vim users that want to look cool.
