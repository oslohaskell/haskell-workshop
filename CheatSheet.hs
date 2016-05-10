module CheatSheet where

import Data.Char

{- Read this through once, then keep it as a reference -}

-- Literals and Data types
character = 'c' :: Char
string = "String" :: String
int = 3 :: Int
float = 4.5 :: Float
boolean = True :: Bool
list = ["element1", "element2"] -- <- all elements in a list must be of the same type

-- Strings
stringConcat = "string1" ++ "string2"
convertToString = show 1234

-- Comparison
equals = 1 == 1
notEquals = 1 /= 2
greaterThan = 1 > 2
gte = 1 >= 2
lessThan = 1 < 2
lte = 1 <= 2

-- Logic
and = True && True
or = True || False
inverse = not True

-- If-then-else
ifThenElse = if 1 == 1
                then "All is well"
                else "What's happening??"

-- Numbers and arithmetic
multiply = 3 * 3
divide = 3 / 3
plus = 3 + 3
minus = 3 - 3

-- Lists
listConcat = [1,2,3] ++ [4,5,6]
listAppend = 1:[2,3,4]
firstElementInList = head [1,2,3]
restOfList = tail [1,2,3]
fifthElementInList = [1,2,3,4,5,6,7] !! 5
lengthOfList = length [1,2,3]

-- Strings are lists of Char
myName = "John" -- Same as myName = ['J', 'o', 'h', 'n']
myNameCapitalized = map toUpper myName -- "JOHN"

-- Tuples
aPair = ("whatever", 1234)
firstElementInPair = fst aPair -- "whatever"
secondElementInPair = snd aPair -- 1234

-- Types
type Name = String -- a type _alias_ for String, makes function signatures easier to read

bestPinkFloydAlbums = [("Animals", 1977), ("The Piper at the Gates of Dawn", 1967)]
years  = map snd bestPinkFloydAlbums -- [1977, 1967]
titles = map fst bestPinkFloydAlbums -- ["Animals", "The Piper at the Gates of Dawn"]

-- Currying
evenNumbers = map (* 2) [1..10] -- [2,4,6,8,10,12,14,16,18,20]

-- Infinite lists and lazyness
allPositiveEvensInTheWorld = map (* 2) [1..]      -- This alone doesn't compute anything ...
get5Evens = take 5 allPositiveEvensInTheWorld     -- .. until it is asked for

-- Printing
printme = putStrLn "I will be printed on the terminal with a '\n' at the end, but not until you evaluate me"

-- Using '$' to avoid parentheses-nightmares
-- The following expressions are equivalent
e1 = head $ take 10 $ filter (> 2) [1..]
e2 = head (take 10 (filter (> 2) [1..]))

-- Zip to pair elements
myzippedlist = zip [1..] ["One", "Two", "Three", "Four"] -- [(1,"One"),(2,"Two"),(3,"Three"),(4,"Four")]

-- Make a finite list infinite with cycle
infiniteLogicalToggle = cycle [True, False] -- take 9 infiniteLogicalToggle -> [True,False,True,False,True,False,True,False,True]

-- Lambdas
numbers = map (\x -> 2 ^ x) [1..10] -- [2,4,8,16,32,64,128,256,512,1024]

-- List comprehensions
s = ["Element #" ++ (show x) | x <- [1..20], x `mod` 3 == 0] -- ["Element #3","Element #6","Element #9","Element #12","Element #15","Element #18"]

-- dropWhile and takeWhile
skipBlanks = dropWhile (== ' ') "    i am indented" -- "i am indented"
justNumbersPlease = takeWhile isNumber "12321421afagad" -- "12321421"
