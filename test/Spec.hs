{-# LANGUAGE TypeApplications #-}

import Number.Benford(cdfToFirstDigit', cdfToNextDigit2', cdfToNextDigit10', firstDigit', firstDigitCdf')

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Options
import Test.Framework.Runners.Options

import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

main :: IO ()
main = defaultMain tests

dig10_1 :: Double
dig10_1 = 0.301

dig10_2 :: Double
dig10_2 = 0.176

dig10_3 :: Double
dig10_3 = 0.125

dig10_4 :: Double
dig10_4 = 0.097

dig10_5 :: Double
dig10_5 = 0.079

dig10_6 :: Double
dig10_6 = 0.067

dig10_7 :: Double
dig10_7 = 0.058

dig10_8 :: Double
dig10_8 = 0.051

dig10_9 :: Double
dig10_9 = 0.046

dig2_1 :: Double
dig2_1 = 1.0

tests = [
    testGroup "Valid probabilities" [
      testProperty "Check probability bounds for firstDigit'" (checkValidProbability firstDigit')
    , testProperty "Check probability bounds for firstDigitCdf'" (checkValidProbability firstDigitCdf')
    ]
  , testGroup "Benford probability tests" [
      testProperty "First digit decimal" (checkProbabilityForRadix 10 1 dig10_1)
    , testProperty "Second digit decimal" (checkProbabilityForRadix 10 2 dig10_2)
    , testProperty "Third digit decimal" (checkProbabilityForRadix 10 3 dig10_3)
    , testProperty "Fourth digit decimal" (checkProbabilityForRadix 10 4 dig10_4)
    , testProperty "Fifth digit decimal" (checkProbabilityForRadix 10 5 dig10_5)
    , testProperty "Sixth digit decimal" (checkProbabilityForRadix 10 6 dig10_6)
    , testProperty "Seventh digit decimal" (checkProbabilityForRadix 10 7 dig10_7)
    , testProperty "Eighth digit decimal" (checkProbabilityForRadix 10 8 dig10_8)
    , testProperty "Nineth digit decimal" (checkProbabilityForRadix 10 9 dig10_9)
    , testProperty "First digit binary" (checkProbabilityForRadix 2 1 dig2_1)
    ]
  , testGroup "Benford cdf to digit tests" [
      testProperty "Check probability of the first digit for the extremes" checkCdfToFirstDigitForRadix
    , testProperty "Check probability of the next digit for the extremes with radix 10" checkCdfToNextDigitForRadix10
    , testProperty "Check probability of the next digit for the extremes with radix 2" checkCdfToNextDigitForRadix2
    ]
  , testGroup "Benford cumulative probability tests" [
      testProperty "cumulative probabilities decimal" (checkCumulativeProbabilityForRadix 10 [dig10_1, dig10_2, dig10_3, dig10_4, dig10_5, dig10_6, dig10_7, dig10_8, dig10_9])
    , testProperty "cumulative probabilities for an arbitrary radix" checkCumulativeProbabilityDistributionForRadix
    , testProperty "cumulative probabilities for an arbitrary radix for the last digit" checkCumulativeProbabilityDistributionForLastDigit
    ]
  , testGroup "Benfords cdf to digit tests" [
      testProperty "cumulative probability to first digit should produce a valid first digit" checkCdfToFirstDigit
    -- , testProperty "cumulative probability to the next digit should produce a valid next digit" checkCdfToNextDigit
    ]
  ]

checkProbabilityForRadix :: Int -> Int -> Double -> Bool
checkProbabilityForRadix radix digit exp = abs (firstDigit' radix digit - exp) <= 0.001

checkValidProbability :: (Int -> Int -> Double) -> Int -> Int -> Bool
checkValidProbability f radix digit = radix < 2 || digit <= 0 || digit >= radix || (0.0 <= prob && prob <= 1.0)
  where prob = f radix digit

checkCumulativeProbabilityForRadix :: Int -> [Double] -> Bool
checkCumulativeProbabilityForRadix radix items = and (zipWith (\d exp -> abs (firstDigitCdf' radix d - exp) <= 0.001) [1 ..] (scanl1 (+) items))

checkCdfToFirstDigitForRadix :: Int -> Bool
checkCdfToFirstDigitForRadix radix = radix < 2 || (cdfToFirstDigit' radix 0.0 == 1 && cdfToFirstDigit' radix 0.9999999 == radix - 1)

checkCdfToNextDigitForRadix10 :: Integer -> Bool
checkCdfToNextDigitForRadix10 prefix = prefix < 1 || (cdfToNextDigit10' prefix 0.0 == 0 && cdfToNextDigit10' prefix 0.9999999 == 9)

checkCdfToNextDigitForRadix2 :: Integer -> Bool
checkCdfToNextDigitForRadix2 prefix = prefix < 1 || (cdfToNextDigit2' prefix 0.0 == 0 && cdfToNextDigit2' prefix 0.9999999 == 1)

checkCumulativeProbabilityDistributionForRadix :: Int -> Bool
checkCumulativeProbabilityDistributionForRadix radix = radix <= 1 || and (zipWith (\d exp -> abs (firstDigitCdf' radix d - exp) <= 0.001) [1 ..] (scanl1 (+) (map (firstDigit' radix) [1 .. radix-1])))

checkCumulativeProbabilityDistributionForLastDigit :: Int -> Bool
checkCumulativeProbabilityDistributionForLastDigit radix = radix <= 1 || abs (firstDigitCdf' radix (radix - 1) - 1.0) <= 0.001

checkCdfToFirstDigit :: Int -> Double -> Bool
checkCdfToFirstDigit radix cdf = radix <= 1 || cdf < 0.0 || cdf > 1.0 || (0 < x && x < radix)
  where x = cdfToFirstDigit' radix cdf

checkCdfToNextDigitForLastDigit :: Int -> Double -> Bool
checkCdfToNextDigitForLastDigit radix cdf = radix <= 1 || cdf < 0.0 || cdf > 1.0 || (0 < x && x < radix)
  where x = cdfToFirstDigit' radix cdf
