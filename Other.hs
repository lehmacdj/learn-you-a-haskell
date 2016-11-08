-- Implementation of a while loop
-- in haskell
-- Excercise from:
-- https://wiki.haskell.org/IO_inside#.27.3E.3E.3D.27_and_.27do.27_notation
while :: IO Bool -> IO ()
while action = do
    guard <- action
    if guard
    then while action
    else return ()
