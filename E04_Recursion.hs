module E04_Recursion where

import WorkshopPrelude

{-
    A recursive function is a recursive function is a recursive function is a
    recursive function is. Very funny.

    Informally we can say that a recursive function calls itself until some
    condition is met and then returns a value.
-}
downToZero x =
    if x > 0
       then downToZero (x - 1)
       else x
{-
    This meaningless function returns 0 by reducing the argument by one each
    time the function is called. As you may have noticed this function is
    flawed. For negative numbers it will never terminate!
    This illustrates an important point about recursive functions: it's your
    resposibility to make sure they terminate. The compiler can't help you with
    that. Remember the halting problem? :)

    Lists and recursion is a marriage made in heaven:
-}
returnLastElementPartial [x] = x
returnLastElementPartial xs = returnLastElementPartial (tail xs)

{-
    This is one way to recurse through a list. Our base case is the singleton
    list (a list of one element). For other lists we recurse on a stricly
    smaller list.

    Exercise:
    You might have noticed that this function is partial: it's not defined for
    the empty list. Depending on our use case this might be a really bad idea.
    Change the function so it returns an element wrapped in `Maybe` (this is
    just a fancy way of saying “`Nothing` or `Just …`”.
-}

returnLastElement :: [a] -> Maybe a
returnLastElement []  = _YOUR_CODE_HERE
returnLastElement [x] = _YOUR_CODE_HERE
returnLastElement xs  = _YOUR_CODE_HERE


{-
    Exercise:
    Define the secondToLast function so it returns the second to last element
    in the input list.  Return `Nothing` if the list is too short. Feel free to
    add more function clauses as you see fit.
-}
secondToLast :: [a] -> Maybe a
secondToLast _ = _YOUR_CODE_HERE

{-
    Exercise:
    Define the listLength function that computes the length of a list.
    Of course, you shouldn't use the pre-defined length function ;)
-}
listLength :: [Int] -> Int
listLength list = _YOUR_CODE_HERE

{-
    While explicit recursion sure works, we can often use generic “recursion
    schemes”. In `RecursionSchemes` we'll look at some of the functional
    programming darlings `map`, `filter`, `fold` and their friends, and see how
    they often can help to do calculations on data structures such as lists.
-}
