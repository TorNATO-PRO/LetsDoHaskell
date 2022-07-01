# How about we do some Haskell?

Attempting the [99 Haskell problems](https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems) (well, apparently there is actually only 88 and I have only done 40) to become a better [Haskell](https://www.haskell.org/) developer. 


#### Some code that I am proud of:

```hs
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome lst = all (uncurry (==)) $ zip lst (rev lst)
```

```hs
encode :: Eq a => [a] -> [(Int, [a])]
encode = map (\a -> (length' a, a)) . pack
```

```hs
-- groups the elements of a set into disjoint subsets
group' :: (Eq a) => [Int] -> [a] -> [[[a]]]
group' [] _ = []
group' [_] lst = [[lst]]
group' (d : ds) lst = [c : g | c <- combinations d lst, g <- group' ds (lst \\ c)]
```

```hs
-- generates the combinations of k distinct object from the n elements of a list
combinations :: Int -> [a] -> [[a]]
combinations _ [] = []
combinations n lst@(x : xs)
  | n <= 0 = []
  | n == 1 = [[a] | a <- lst]
  | otherwise = [x : a | a <- combinations (n - 1) xs] ++ combinations n xs
```

---

Enjoy, and make sure to try these out yourself! If looking at this hasn't convinced you to start trying to learn Haskell, here is perhaps a [better resource](https://wiki.haskell.org/Why_Haskell_matters). There is a reason behind the relatively recent functional programming fad, which is also something that [you should look into](http://www.cse.chalmers.se/~rjmh/Papers/whyfp.html). It is not just smug Vim users that want to look cool.
