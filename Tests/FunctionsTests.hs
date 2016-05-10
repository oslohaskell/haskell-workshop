module Tests.FunctionsTests where

import WorkshopPrelude
import qualified Test.QuickCheck           as QC
import qualified E02_Functions              as FU
import Test.Hspec

tests = do

    describe "Functions.multiply10by20" $ do
        it "returns 200" $ do
            FU.multiply10by20 `shouldBe` 200

    describe "Functions.plus" $ do
        it "adds to *arbitrary* numbers" $ do
            QC.property $ \x y -> FU.plus x y == x + y

    describe "Functions.sum3" $ do
        it "adds three *arbitrary* numbers" $ do
            QC.property $ \x y z -> FU.sum3 x y z == x + y + z

    describe "Functions.isDollar" $ do
        it "True only for '$'" $ do
            QC.property $ \c -> if c == '$' then FU.isDollar c else FU.isDollar c == False

    describe "Functions.xor" $ do
        it "xor works as expected" $ do
            [FU.xor False False,
             FU.xor False True,
             FU.xor True  False,
             FU.xor True  True] == [False, True, True, False]

main = hspec tests

