import Data.Char
import Control.Monad

main0 = do
    putStrLn "Hello world!"

main1 = do
    putStrLn "What's your first name?"
    firstName <- fmap (map toUpper) getLine
    putStrLn "What's your last name?"
    lastName <- fmap (map toUpper) getLine
    putStrLn $ "Hello " ++ firstName ++ " " ++ lastName ++ "!"

main2 = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

main3 = do
    c <- getChar
    if c /= ' '
        then do
            putChar c
            main
        else
            putChar '\n'

-- More optimized version of main3
main4 = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        main

main5 = do
    lines <- sequence $ replicate 4 getLine
    print lines

main6 = do
    colors <- forM [1..4] (\a -> do
        putStrLn $ "What color do you associate with the number " ++ show a ++ "?"
        getLine)
    putStrLn $ "You associcate the following colors with 1, 2, 3, and 4:"
    mapM_ putStrLn colors

-- Capitalize
main7 = do
    contents <- getContents
    putStr $ (map toUpper contents)

-- Short lines only
main8 = interact $ unlines . filter ((<10) . length) . lines

-- Palindrome
main9 = interact $
    unlines
    . map (\line -> if reverse line == line
        then "is a palindrome"
        else "not a palindrome")
    . lines

-- Todo program is in todo.hs

-- Random functionallity from System.Random

-- use `catch` from System.IO.Error
