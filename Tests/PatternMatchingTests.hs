module Tests.PatternMatchingTests where

import WorkshopPrelude
import qualified Test.QuickCheck    as QC
import qualified E07_PatternMatching as PM
import Test.Hspec

tests = do

    describe "PatternMatching.secondElement" $ do
        let input = [1,2,3,4,5]
            expected = 2
         in it ("returns the second element from " ++ show input) $ do
            PM.secondElement input `shouldBe` expected

    describe "PatternMatching.drop3" $ do
        let input = [1,2,3,4,5,6]
            expected = [4,5,6]
         in it ("drops the three first elements in " ++ show input) $ do
            PM.drop3 input `shouldBe` expected

    describe "PatternMatching.thirdAndLast" $ do
        let input = [1,2,3]
            expected = 3
         in it ("returns the third element in " ++ show input) $ do
            PM.thirdAndLast input `shouldBe` expected


main = hspec tests

