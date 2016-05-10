{-# LANGUAGE ScopedTypeVariables #-}
module Tests.PartialApplicationTests where

import WorkshopPrelude
import Data.Char (isDigit)
import qualified Test.QuickCheck      as QC
import qualified E03_PartialApplication as PA
import Test.Hspec

tests = do
    describe "PartialApplication.addTwo" $
        it "increments all elements by two" $
            QC.property $ \(xs :: [Int]) -> map (+2) xs == PA.addTwo xs

    describe "PartialApplication.containsZero" $
        it "checks that a zero is detected" $
            QC.property $ \(xs :: [Int]) -> (0 `elem` xs) == PA.containsZero xs

    describe "PartialApplication.isDigit" $
        it "checks that isDigit correctly classifies chars" $
            QC.property $ \(c :: Char) -> isDigit c == PA.isDigit c

main = hspec tests
