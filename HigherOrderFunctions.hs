divBy10 :: (Fractional a) => a -> a
divBy10 = (/10)

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (h:t) = f h : map f t

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (h:t)
    | p h = h : filter' p t
    | otherwise = filter' p t

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (h:t) =
    let smallSorted = quicksort $ filter (<=h) t
        bigSorted = quicksort $ filter (>h) t
    in  smallSorted ++ [h] ++ bigSorted

-- The smallest number that is divisible by [1..10]
bigestDivisible :: (Integral a) => a
bigestDivisible = head $ filter divisibleBy1to10 [1..]
    where x `divides` y = y `mod` x == 0
          divisibleBy1to10 x = all (`divides`x) [1..10]

-- Two different approaches for sum of all odd square numbers less than 10,000
_ = sum $ takeWhile (<=10000) $ filter odd $ map (^2) [1..]
_ = sum $ takeWhile (<=10000) [x^2 | x <- [1..], odd (x^2)]

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | odd n = n : chain (3 * n + 1)
    | even n = n : chain (n `div` 2)

numLongChains :: Int
numLongChains = length $ filter isLong $ map chain [1..100]
    where isLong xs = length xs > 15

sum' :: (Num a, Foldable t) => t a -> a
sum' = foldl (+) 0
