lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky _ = "Sorry, you're out of luck, pal!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

head' :: [a] -> a
head' [] = error "No calling head on an empty list!"
head' (hd:_) = hd

length_rec :: (Num b) => [a] -> b
length_rec [] = 0
length_rec (_:tl) = 1 + length_rec tl

length_monad :: (Num b) => [a] -> b
length_monad xs = sum [1 | _ <- xs]

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (hd:tl) = hd + sum' tl
