maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Maximum on an empty list!"
maximum' [x] = x
maximum' (h:t)
    | h > tMax  = h
    | otherwise = tMax
    where tMax = maximum' t

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x : replicate' (n - 1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' n [] = []
take' n (h:t) = h : take' (n - 1) t

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (h:t) =
       (quicksort [x | x <- t, x < h])
    ++ [h]
    ++ (quicksort [x | x <- t, x >= h])
