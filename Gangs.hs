import Data.Monoid
import Control.Monad.Writer

type Food = String
type Price = Sum Int

-- This is an introduction to the reader monad
isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)
applyLog (x,log) f = let (y,newLog) = f x in (y,log `mappend` newLog)

addDrink :: Food -> (Food,Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink "beer" = ("beer", Sum 30)

-- Can't get the type of MonadWriter to line up correctly!
