module Main (main) where

import Test.Tasty
import Test.Tasty.Hspec

import ProductSpecs

main :: IO ()
main = do
  specs <- concat <$> mapM testSpecs [productSpecs]
  defaultMain $ testGroup "All Tests" [testGroup "Specs" specs]
