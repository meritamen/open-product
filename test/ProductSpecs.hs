{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}

module ProductSpecs where

import Data.Product
import Test.Hspec

productSpecs :: Spec
productSpecs = do
  describe "OpenProduct" $ do
    it "insert and get Int" $
      get #IntKey (insert #DoubleKey (Just @Double 8.964) (insert #IntKey (Just 10) nil)) `shouldBe` Just @Int 10
    it "insert and update double" $ do
      get #DoubleKey (update #DoubleKey (Just @Double 2.952)(insert #DoubleKey (Just @Double 8.964) (insert #IntKey (Just @Int 10) nil))) `shouldBe` Just @Double 2.952
    it "insert, delete, insert, and get Int" $ do
      get #IntKey (insert #IntKey (Just 20) (delete #IntKey (insert #IntKey (Just @Int 10) nil))) `shouldBe` Just @Int 20
    it "upsert and get Int" $
      get #IntKey (upsert #DoubleKey (Just @Double 8.964) (insert #IntKey (Just 10) nil)) `shouldBe` Just @Int 10
