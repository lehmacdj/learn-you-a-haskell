removeNonUpperCase :: [Char] -> [Char]
removeNonUpperCase st = [c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circmference r = 2 * pi * r

circumference :: Double -> Double
circmference r = 2 * pi * r


