import qualified Data.Map as M
import Control.Monad
import Text.Read
-- A reverse polish notation calculator implemented functionally

solveRPN :: (Floating a, Read a) => String -> Maybe a
solveRPN = head . foldl foldingFunction [] . words

foldingFunction :: (Floating a, Read a) => [Maybe a] -> String -> [Maybe a]
foldingFunction (x:y:ys) op = ((op `lookup` binary) <*> y <*> x) : ys
foldingFunction (x:xs) op = ((op `lookup` unary) <*> x) : xs
foldingFunction xs "sum" = [(liftM sum) $ sequence xs]
foldingFunction xs num = readMaybe num : xs

unary :: Floating a => [(String, a -> a)]
unary = [ ("ln", log)
        , ("sin", sin)
        , ("cos", cos)
        , ("tan", tan)
        ]

binary :: Floating a => [(String, a -> a -> a)]
binary = [ ("+", (+))
         , ("-", (-))
         , ("*", (*))
         , ("/", (/))
         , ("^", (**))
         ]
