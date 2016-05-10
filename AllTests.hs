module Main where

{-
    You're not supposed to touch this ;)
-}

import WorkshopPrelude
import Test.QuickCheck
import Test.Hspec

import qualified Tests.GettingStartedTests            as GS
import qualified Tests.FunctionsTests                 as FU
import qualified Tests.PartialApplicationTests        as PA
import qualified Tests.RecursionTests                 as RE
import qualified Tests.RecursionSchemesTests          as RS
import qualified Tests.CreditCardNumberValidatorTests as CV
import qualified Tests.PatternMatchingTests           as PM
import qualified Tests.HigherOrderFunctionsTests      as HF
import qualified Tests.ProjectEulerTests              as PE
import qualified Tests.QuickCheckExamplesTests        as QC
import qualified Tests.VigenereTests                  as VI


main :: IO ()
main = hspec $ do
    GS.tests
    FU.tests
    PA.tests
    RE.tests
    RS.tests
    CV.tests
    PM.tests
    HF.tests
    PE.tests
    QC.tests
    VI.tests
