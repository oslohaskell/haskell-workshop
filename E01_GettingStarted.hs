module E01_GettingStarted where

import WorkshopPrelude

-- Let's get one thing out of the way: I'm a comment!

{-
  So am I, but I can span
  multiple
  lines
-}



{-
    The 'main' function is the entry point of the program
    Try running this by setting this module as the current target
    and hitting Ctrl+Enter
-}
main = putStrLn "Hello World"

{-
    Let's break that down:

        * `main` is a function definition. It doesn't take any arguments, and prints something to the console.

        * `putStrLn` is a function that prints a given string to the console

-}

{-
   Now the very first "exercise" is to change the `myName` function to
   return your name:
-}
myName = "Finn Finnsen"

{-
  ... then change the target to `Tests` and hit Run again.
  You passed a test, right? Congrats.
-}
