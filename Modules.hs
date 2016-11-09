import Data.List
import Data.Function (on)
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Geometry.Sphere as Sphere
import qualified Geometry.Cuboid as Cuboid
import qualified Geometry.Cube as Cube

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

phoneBook =
    [("betty", "555-2938")
    ,("bonnie", "452-2928")
    ,("patsy", "493-2928")
    ,("lucile", "205-2928")
    ,("wendy", "939-8282")
    ,("penny", "853-2492")
    ]

-- this is equivalent to lookup from Data.List
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key = foldl (\a (k,v) -> if key == k then Just v else a) Nothing

text1 = "I just had an anime dream. Anime... Reality... Are they so different?"
text2 = "The old man left his garbage can out and now his trash is all over my lawn!"

-- The remainder of modules is found in Geometry/

unitSphereSA = Sphere.area 1
