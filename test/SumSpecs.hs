{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module SumSpecs where

import Data.Extensible.Sum
import Data.Functor.Identity
import Test.Hspec

type DummySum = OpenSum Identity '[Int, Bool, String]
type DummySum' = OpenSum Identity '[Double, Int, Bool, String]

sumSpecs :: Spec
sumSpecs = do
  describe "OpenSum" $ do
    let
      exInt = inj @_ @Int @[Int, Bool, String] (Identity 42)
      exInt' = weaken exInt :: DummySum'
      exBool = inj @_ @Bool   @[Int, Bool, String] (Identity True)
      exString = inj @_ @String @[Int, Bool, String] (Identity "s t r i n g")
    it "injects and projects Int" $ do
      prj @_ @Int @[Int, Bool, String] exInt `shouldBe` Just (Identity 42)
    it "fails to project wrong type" $ do
      prj @_ @Bool @[Int, Bool, String] exInt `shouldBe` Nothing
    it "injects and projects String" $ do
      prj @_ @String @[Int, Bool, String] exString `shouldBe` Just (Identity "s t r i n g")
    it "decomposes head" $ do
      case decompose exInt of
        Left (Identity 42) -> pure ()
        Left _             -> expectationFailure "Decomposed value didn't match Identity 42"
        Right _            -> expectationFailure "Decomposition did not yield Left"
    it "decomposes tail" $ do
      case decompose exBool of
        Left _ -> expectationFailure "Expected Right"
        Right (UnsafeOpenSum idx _) -> idx `shouldBe` 0
    it "weakens an OpenSum" $ do
      prj @_ @Int @[Double, Int, Bool, String] exInt' `shouldBe` Just (Identity 42)
    it "match returns a constant for any type" $ do
      match (const ()) exInt `shouldBe` ()
      match (const ()) exBool `shouldBe` ()
      match (const ()) exString `shouldBe` ()
