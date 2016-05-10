module Tests.RecursionTests where

import WorkshopPrelude
import qualified Test.QuickCheck as QC
import qualified E04_Recursion    as RE
import Test.Hspec

tests = do

    describe "Recursion.returnLastElement I" $ do
      let input = [1,2,3]
          expected = Just 3
      it ("returns the last element in " ++ show input) $ do
        RE.returnLastElement input `shouldBe` expected

    describe "Recursion.returnLastElement II" $ do
      let input = ([] :: [Int])
          expected = Nothing :: Maybe Int
      it "returns Nothing for []" $ do
        RE.returnLastElement input `shouldBe` expected

    describe "Recursion.secondToLast I" $ do
        let input = [1,2,3,4]
            expected = Just 3
        it ("returns the second to last element in " ++ show input) $ do
            RE.secondToLast input `shouldBe` expected

    describe "Recursion.secondToLast II" $ do
        let input = ([] :: [Int])
            expected = Nothing :: Maybe Int
        it "returns Nothing for []" $ do
            RE.secondToLast input `shouldBe` expected

    describe "Recursion.listLength" $ do
        it "returns the length of an *arbitrary* list" $ do
            QC.property $ \x -> RE.listLength x == length x

main = hspec tests

