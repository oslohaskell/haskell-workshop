module E03_PartialApplication where

import WorkshopPrelude

{-
    The following function `add` can be thought of as taking only one parameter
    and then returning a function that takes one parameter.  Supplying fewer
    arguments thand the function takes, and getting back a new function that
    takes the remaining argument(s) is called partial application.
-}

add :: Int -> Int -> Int
add x y = x + y

{-
    The function `add` takes two arguments.
    Use partial application of `add` to add 2 to every element in a list,
    e.g. turn [1,2,3] into [3,4,5]:
-}

addTwo xs = map (_YOUR_CODE_HERE) xs

{-
    The function `elem` takes a value and a list of values of that type, and
    returns True if the given value is in the list:
    elem 3 [1,2,3]   = True
    elem 4 [1,2,3]   = False
    'a' `elem` "abc" = True   (note: "abc" is equal to the list ['a','b','c'])

    Use partial application of `elem` to create a function that takes a list
    of integers and returns True if it contains the number 0.
-}

containsZero :: [Int] -> Bool
containsZero = _YOUR_CODE_HERE

{-
    Now, let's turn the parameters around. Can you create a function that
    takes a Char (e.g. 'k') and checks whether it is a digit using partial
    application of `elem`? (You can create a list of the possible digits
    using `['0'..'9']` or simply `"0123456789"`.
-}

isDigit :: Char -> Bool
isDigit = _YOUR_CODE_HERE
