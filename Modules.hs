import Data.List
import Data.Function (on)
import Data.Char

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

fibonaccis :: (Integral a) =>  [a]
fibonaccis = map fst $ iterate (\(x, y) -> (y, x + y)) (1, 1)

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency = map (\l@(h:_) -> (h, length l)) . group . sort

-- This is basically just an implementation of isInfixOf
containsSublist :: (Eq a) => [a] -> [a] -> Bool
haystack `containsSublist` needle =
    let nlen = length needle
    in  any (\t -> take nlen t == needle) $ tails haystack

listLengthFrequency :: [[a]] -> [(Int, Int)]
listLengthFrequency = map (\l@(h:_) -> (length h, length l))
    . groupBy ((==) `on` length)
    . sortBy (compare `on` length)

wordLengthFrequncy :: String -> [(Int, Int)]
wordLengthFrequncy = listLengthFrequency . words

decToHex :: Int -> String
decToHex n
    | n < 0 = error "Cannot convert numbers that are < 0"
    | n < 16 = [toUpper $ intToDigit n]
    | otherwise = decToHex (n `div` 16) ++ decToHex (n `mod` 16)

encode :: Int -> String -> String
encode shift msg =
    let ords = map ord msg
        shifted = map (+ shift) ords
    in  map chr shifted

decode :: Int -> String -> String
decode shift = encode (negate shift)

-- TODO: Data.Map
