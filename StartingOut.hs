doubleMe x = x + x

doubleUs x y = 2 * x + 2 * y

doubleSmallNumber x = if x > 100
                        then x
                        else 2 * x

doubleSmallNumber' x = (if x > 100 then x else 2 * x) + 1

conanO'Brien = "It's a-me, Conan O'Brien!"

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
