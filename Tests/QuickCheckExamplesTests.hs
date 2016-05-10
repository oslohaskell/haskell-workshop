module Tests.QuickCheckExamplesTests where

import WorkshopPrelude
import qualified Test.QuickCheck       as QC
import qualified E10_QuickCheckExamples as QCE
import Test.Hspec

tests = do

    describe "QuickCheckExamples.encode" $ do
        it "encodes \"Heeeello Haskell!!!\" as [(1,'H'),(4,'e'),(2,'l'),(1,'o'),(1,' '),(1,'H'),(1,'a'),(1,'s'),(1,'k'),(1,'e'),(2,'l'),(3,'!')]" $ do
            QCE.encode "Heeeello Haskell!!!" `shouldBe` [(1,'H'),(4,'e'),(2,'l'),(1,'o'),(1,' '),(1,'H'),(1,'a'),(1,'s'),(1,'k'),(1,'e'),(2,'l'),(3,'!')]

    describe "QuickCheckExamples.decode" $ do
        it "decodes [(1,'H'),(4,'e'),(2,'l'),(1,'o'),(1,' '),(1,'H'),(1,'a'),(1,'s'),(1,'k'),(1,'e'),(2,'l'),(3,'!')] as \"Heeeello Haskell!!!\"" $ do
            QCE.decode [(1,'H'),(4,'e'),(2,'l'),(1,'o'),(1,' '),(1,'H'),(1,'a'),(1,'s'),(1,'k'),(1,'e'),(2,'l'),(3,'!')] `shouldBe` "Heeeello Haskell!!!"

    describe "QuickCheckExamples.encodeDecode" $ do
        it "decode is inverse encode for any string" $ do
            QC.property $ \xs -> xs == (QCE.decode . QCE.encode) xs

main = hspec tests

