module Main (main) where

import Test.Tasty
import Test.Tasty.Hspec

import ProductSpecs
import SumSpecs

main :: IO ()
main = do
  specs <- concat <$> mapM testSpecs [productSpecs, sumSpecs]
  defaultMain $ testGroup "All Tests" [testGroup "Specs" specs]
