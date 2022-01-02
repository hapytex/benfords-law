{-# LANGUAGE TypeApplications #-}

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Options
import Test.Framework.Runners.Options

import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

main :: IO ()
main = defaultMain tests

tests = [
    testGroup "Benford radix 10 tests" [
    ]
  ]
