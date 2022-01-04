{-# LANGUAGE BangPatterns, TypeApplications #-}

{-|
Module      : Number.Benford
Description : Determining probabilities and generating sequences according to Benford's law.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

This module makes it possible to calculate probabilities and generate sequences specified by /Benford's law/.
-}

module Number.Benford (
    -- * Probabilities of the first digit
    firstDigit, firstDigit10
  , firstDigit', firstDigit10'
    -- * Cumulative density for the first digit
  , firstDigitCdf, firstDigitCdf10
  , firstDigitCdf', firstDigitCdf10'
    -- * Probabilities of numbers starting with a sequence of digits
  , startSequence', startSequence2', startSequence10'
  , startSequence, startSequence2, startSequence10
    -- * Reverse cumulative density function for the first digit
  , cdfToFirstDigit, cdfToFirstDigit10
  , cdfToFirstDigit', cdfToFirstDigit10'
    -- * Reverse cumulative density function for the next digits
  , cdfToNextDigit, cdfToNextDigit2, cdfToNextDigit10
  , cdfToNextDigit', cdfToNextDigit2', cdfToNextDigit10'
    -- * Generate first digits
  , generateFirstDigit, generateFirstDigit', generateFirstDigit10
    -- * Generate the next digits
  , generateNextDigit, generateNextDigit', generateNextDigit2, generateNextDigit10
    -- * Generate Benford sequences
  , generateBenfordSequence, generateBenfordSequence2, generateBenfordSequence10
  ) where

import Control.Arrow(first)

import Data.Foldable(foldl')
import Data.Ratio(numerator, denominator)

import System.Random(Random, RandomGen, random)

_generatorMapping :: (RandomGen g, Random a)
  => (a -> b)
  -> g
  -> (b, g)
_generatorMapping f = go
  where go = first f . random

_baseFunction :: Floating a => Int -> Integer -> a
_baseFunction _ 0 = 1
_baseFunction r n = logBase (fromIntegral r) (1 + 1 / fromInteger n)

_baseFunction' :: (Floating a, Integral i) => a -> i -> a
_baseFunction' _ 0 = 1
_baseFunction' r n = logBase r (1 + 1 / fromIntegral n)


_baseCdfToNextDigit :: (Floating a, RealFrac a, Integral i) => Int -> i -> a -> i
_baseCdfToNextDigit radix prefixSequence cprob = fromInteger (div (fromIntegral prefixSequence * numerator logs) (denominator logs) `mod` fromIntegral radix)
  where scaler = logBase (fromIntegral radix) ((fromIntegral prefixSequence + r) / pref)
        pref = fromIntegral (max 1 prefixSequence)
        r = fromIntegral radix
        logs = toRational (r ** (scaler * cprob))

_radixCheck :: (Int -> a -> Maybe b) -> Int -> a -> Maybe b
_radixCheck f radix
  | radix > 1 = f radix
  | otherwise = const Nothing

_firstDigitCheck :: (Int -> Int -> b) -> Int -> Int -> Maybe b
_firstDigitCheck f radix digit
  | digit < 1 || digit >= radix = Nothing
  | otherwise = Just (f radix digit)

_probabilityCheck :: (Floating a, Ord a) => (Int -> a -> b) -> Int -> a -> Maybe b
_probabilityCheck f radix cprob
  | cprob < 0.0 = Nothing
  | cprob >= 1.0 = Nothing
  | otherwise = Just (f radix cprob)

_fromDigits :: Int -> [Int] -> Integer
_fromDigits radix = foldl' go 0
  where go q = (r*q +) . fromIntegral
        r = fromIntegral radix

_fromDigits' :: Int -> [Int] -> Maybe Integer
_fromDigits' radix = go 0
  where r = fromIntegral radix
        go !n [] = Just n
        go !n (x:xs)
          | x < 0 || x >= radix = Nothing
          | otherwise = go (r*n + fromIntegral x) xs

-- | Determine the probability of the first digit for a /decimal/ system.
firstDigit10 :: Floating a
  => Int  -- ^ The given digit for which we determine the probability, should be greater than zero and less than 10.
  -> Maybe a  -- ^ The probability for the given digit to be the first digit in a number wrapped in a 'Just'; 'Nothing' if the digit is invalid.
firstDigit10 = firstDigit 10

-- | Determine the probability for a given digit to be the first digit for a number in a number system with a given radix.
firstDigit :: Floating a
  => Int  -- ^ The given radix, should be greater than or equal to two.
  -> Int  -- ^ The given digit to determine the probability. Should be greater than zero and less than the radix.
  -> Maybe a  -- ^ The probability for the given digit to be the first digit in a number system with the given radix wrapped in a 'Just'; 'Nothing' if the radix or digit are invalid.
firstDigit = _radixCheck (_firstDigitCheck firstDigit')

-- | Determine the probability of the first digit in a /decimal/ system.
firstDigit10' :: Floating a
  => Int  -- ^ The given digit to determine the probability of.
  -> a  -- ^ The probability for the given digit to be the first digit of a number. Unspecified behavior in case the digit is not between 1 and 9.
firstDigit10' = firstDigit' 10

-- | Determine the probability of the first digit for a number system with a given radix.
firstDigit' :: Floating a
  => Int  -- ^ The given /radix/, should be greater than or equal to two.
  -> Int  -- ^ The given /digit/, should be greater than zero and less than the radix.
  -> a  -- ^ The probability of the given digit being the first digit for a number in a number system for the given radix, unspecified behavior if the radix or digit do not satisfy the conditions.
firstDigit' radix digit = logBase (fromIntegral radix) ((d + 1) / d)
    where d = fromIntegral digit

-- | Determine the /cumulative distribution function/ for the first digit in a decimal number system.
firstDigitCdf10' :: Floating a
  => Int  -- ^ The first digit for which we determine the cumulative distribution function, the digit should be greater than zero and less than ten.
  -> a  -- ^ The probability of the digit being less than or equal to the given value. For invalid parameters, this is unspecified behavior.
firstDigitCdf10' = firstDigitCdf' 10

-- | Determine the /cumulative distribution function/ for the first digit in a number system with the given radix.
firstDigitCdf' :: Floating a
  => Int  -- ^ The given radix of the number system for which we determine the first digit.
  -> Int  -- ^ The first digit for which we determine the cumulative distribution function, the digit should be greater than zero and less than the radix.
  -> a  -- ^ The probability of the digit being less than or equal to the given value. For invalid parameters, this is unspecified behavior.
firstDigitCdf' radix digit = logBase (fromIntegral radix) (fromIntegral (digit+1))

-- | Determine the /cumulative distribution function/ for the first digit in a decimal number system.
firstDigitCdf10 :: Floating a
  => Int  -- ^ The first digit for which we determine the cumulative distribution function, the digit should be greater than zero and less than ten.
  -> Maybe a  -- ^ The probability of the digit being less than or equal to the given value wrapped in a 'Just'. If the digit is invalid, 'Nothing' is returned.
firstDigitCdf10 = firstDigitCdf 10

-- | Determine the /cumulative distribution function/ for the first digit in a number system with the given radix.
firstDigitCdf :: Floating a
  => Int  -- ^ The given radix of the number system for which we determine the first digit.
  -> Int  -- ^ The first digit for which we determine the cumulative distribution function, the digit should be greater than zero and less than the radix.
  -> Maybe a  -- ^ The probability of the digit being less than or equal to the given value wrapped in a 'Just'. If the radix or the digit is invalid, 'Nothing' is returned.
firstDigitCdf = _radixCheck (_firstDigitCheck firstDigitCdf')

-- | Determine the probability of the sequence starting with the given digits. The leading zeros are ignored. If you
-- thus call @startSequence10' 1425@, you obtain the probability of a number starting with @1@, @4@, @2@, and @5@ as digits.
startSequence10' :: (Integral i, Floating a)
  => i  -- ^ The 'Integral' number that contains the start sequence (with an optional sequence of "leading zeros"), should be greater than zero.
  -> a  -- ^ The probability of a number to start with the given start sequence. Unspecified if the number is less than zero.
startSequence10' = _baseFunction' 10

-- | Determine the probability of the sequence starting with the given digits of a binary number. Binary numbers always start
-- with @1@. If you thus call @startSequence2' 9@, you get the probability of a binary number starting with @1@, @0@, @0@, and @1@
-- as binary digits.
startSequence2' :: (Integral i, Floating a)
  => i  -- ^ The 'Integral' binary number that contains the binary start sequence (with an optional sequence of "leading zeros"), should be greater than zero.
  -> a  -- ^ The probability of a binary number starting with the givenn binary sequence. Unspecified if the number is less than zero.
startSequence2' = _baseFunction' 2

-- | Determine the probability of the sequence starting with the given digits. The leading zeros are ignored. If you
-- thus call @startSequence10' 1425@, you obtain the probability of a number starting with @1@, @4@, @2@, and @5@ as digits.
-- The result is wrapped in a 'Just'; if the value is less than zero, 'Nothing' is returned.
startSequence10 :: (Integral i, Floating a)
  => i  -- ^ The 'Integral' number that contains the start sequence (with an optional sequence of "leading zeros"), should be greater than zero.
  -> Maybe a  -- ^ The probability of a number starting with the given sequence wrapped in a 'Just'. If the given sequence is not valid, 'Nothing' is returned.
startSequence10 n
  | n >= 0 = Just (startSequence10' n)
  | otherwise = Nothing

-- | Determine the probability of the sequence starting with the given digits of a binary number. Binary numbers always start
-- with @1@. If you thus call @startSequence2' 9@, you get the probability of a binary number starting with @1@, @0@, @0@, and @1@
-- as binary digits. The result is wrapped in a 'Just'; if the value is less than zero, 'Nothing' is returned.
startSequence2 :: (Integral i, Floating a)
  => i  -- ^ The 'Integral' binary number that contains the binary start sequence (with an optional sequence of "leading zeros"), should be greater than zero.
  -> Maybe a  -- ^ The probability of a Benford number to start with the given digit wrapped in a 'Just'; 'Nothing' if the given binary number is out of range.
startSequence2 n
  | n >= 0 = Just (startSequence2' n)
  | otherwise = Nothing

-- | Determine the probability of a number in a number system with the given radix, to start with the given sequence of digits.
-- Leading zeros are ignored and the digits are assumed to be valid digits (greater than or equal to zero, and less than the radix).
startSequence' :: Floating a
  => Int  -- ^ The given radix of the number system, should be greater than one.
  -> [Int]  -- ^ The given sequence of digits to deterime the probability for. Leading zeros are ignored, the digits should be greater than or equal to zero and less than the radix.
  -> a  -- ^ The probability of the given digit sequence in a number system with the given radix. Unspecified behavior in case the radix or digits are not valid.
startSequence' radix = _baseFunction radix . _fromDigits radix

-- | Determine the probability of a number in a number system with the given radix to start with the given sequence of digits, leading zeros are ignored.
-- The function will return a 'Nothing' in case the radix or the digit sequence is invalid.
startSequence :: Floating a
  => Int  -- ^ The given /radix/ of the number system, must be greater than one.
  -> [Int]  -- ^  The sequence of digits in the given number system. Leading zeros are ignored, all digits must be greater than or equal to zero, and less than the radix.
  -> Maybe a  -- ^ The probability of the given digit sequence in a number system with the given radix wrapped in a 'Just'. 'Nothing' if the given radix or digit sequence is invalid.
startSequence = _radixCheck go
  where go radix ns
          | Just n <- _fromDigits' radix ns, n >= 0 = Just (_baseFunction radix n)
          | otherwise = Nothing

-- | Determine the digit for a given cumulative probability for a decimal number system.
-- The probability should be greater than or equal to zero, and less than one.
cdfToFirstDigit10' :: (Floating a, RealFrac a)
  => a  -- ^ The given /cumulative probability/, should be greater than or equal to zero, and less than one.
  -> Int  -- ^ The smallest digit for which the cumulative probability is less than the given probability. Unspecified behavior if the radix or cumulative probability are out of range.
cdfToFirstDigit10' = cdfToFirstDigit' 10

-- | Determine the digit for a given cumulative probability for a number system with a given radix.
-- The probability should be greater than or equal to zero, and less than one.
cdfToFirstDigit' :: (Floating a, RealFrac a)
  => Int  -- ^ The given /radix/, should be greater than one.
  -> a  -- ^ The given /cumulative probability/, should be greater than or equal to zero, and less than one.
  -> Int  -- ^ The smallest digit for which the cumulative probability is less than the given probability. Unspecified behavior if the radix or cumulative probability are out of range.
cdfToFirstDigit' radix cprob = floor (fromIntegral radix ** cprob)

-- | Determine the digit for a given cumulative probability for a decimal number system.
-- The probability should be greater than or equal to zero, and less than one.
cdfToFirstDigit10 :: (Ord a, Floating a, RealFrac a)
  => a  -- ^ The given /cumulative probability/, should be greater than or equal to zero, and less than one.
  -> Maybe Int  -- ^ The smallest digit for which the cumulative probability is less than the given probability wrapped in a 'Just'. 'Nothing' if the radix or cumulative probability are out of range.
cdfToFirstDigit10 = cdfToFirstDigit 10

-- | Determine the digit for a given cumulative probability for a number system with a given radix.
-- The probability should be greater than or equal to zero, and less than one.
cdfToFirstDigit :: (Ord a, Floating a, RealFrac a)
  => Int  -- ^ The given /radix/, should be greater than one.
  -> a  -- ^ The given /cumulative probability/, should be greater than or equal to zero, and less than one.
  -> Maybe Int  -- ^ The smallest digit for which the cumulative probability is less than the given probability wrapped in a 'Just'. 'Nothing' if the radix or cumulative probability are out of range.
cdfToFirstDigit = _radixCheck (_probabilityCheck cdfToFirstDigit')

-- | A random number generator that generates the first digit according to Benford's law for a decimal number system.
generateFirstDigit10 :: RandomGen g
  => g  -- ^ The random number generator.
  -> (Int, g)  -- ^ A 2-tuple with the first digit of a number as first item, and the modified random generator as second item.
generateFirstDigit10 = generateFirstDigit' 10

-- | A random number generator that generates the first digit according to Benford's law for a number system with a given radix.
generateFirstDigit' :: RandomGen g
  => Int  -- ^ The radix of the given number system, should be greater than one.
  -> g  -- ^ The random number generator.
  -> (Int, g)  -- ^ A 2-tuple with the first digit of a number as first item, and the modified random generator as second item.
generateFirstDigit' radix = _generatorMapping (min (radix-1) . cdfToFirstDigit' @Double radix)

-- | A conditional random generator that generates the first digit according to Benford's law for a number system with a given radix.
generateFirstDigit :: RandomGen g
  => Int  -- ^ The radix of the given number system, should be greater than one.
  -> g  -- ^ The random number generator that is used to generate the first digit.
  -> Maybe (Int, g)  -- ^ A generator for the first digit wrapped in a 'Just'; 'Nothing' if the radix is out of range.
generateFirstDigit radix
  | radix > 1 = Just . generateFirstDigit' radix
  | otherwise = const Nothing

-- | Convert a given cumulative probability to the corresponding digit after the given prefix for the decimal number system.
cdfToNextDigit10 :: (Floating a, RealFrac a)
  => Int  -- ^ The given /prefix/, leading zeros are ignored. If the prefix is @31@, we thus detrmine the digit after @3@ and @1@. The prefix should be greater than or equal to zero.
  -> a  -- ^ The given cumulative probability to map on a digit for a decimal number system; should be greater than or equal to zero, and less than one.
  -> Maybe Int  -- ^ The corresponding digit for the given prefix and cumulative probability wrapped in a 'Just'; 'Nothing' if the prefix or the cumulative probability are invalid.
cdfToNextDigit10 prefixSequence probability
  | prefixSequence >= 0, probability >= 0.0, probability < 1.0 = Just (cdfToNextDigit10' prefixSequence probability)
  | otherwise = Nothing

-- | Convert a given cumulative probability to the corresponding digit after the given prefix for the binary number system.
cdfToNextDigit2 :: (Floating a, RealFrac a)
  => Int  -- ^ The given /prefix/, leading zeros are ignored. If the prefix is @9@, we thus detrmine the digit after @1@, @0@, @0@ and @1@. The prefix should be greater than or equal to zero.
  -> a  -- ^ The given cumulative probability to map on a digit for a binary number system; should be greater than or equal to zero, and less than one.
  -> Maybe Int  -- ^ The corresponding digit for the given prefix and cumulative probability wrapped in a 'Just'; 'Nothing' if the prefix or the cumulative probability are invalid.
cdfToNextDigit2 prefixSequence probability
  | prefixSequence >= 0, probability >= 0.0, probability < 1.0 = Just (cdfToNextDigit2' prefixSequence probability)
  | otherwise = Nothing

-- | Convert a given cumulative probability to the corresponding digit after the given prefix of digits for a number system with a given radix.
cdfToNextDigit :: (Floating a, RealFrac a)
  => Int  -- ^ The given radix of the number system, should be greater than one.
  -> [Int]  -- ^ The list of prefix digits, leading zeros are ignored, the digits should all be greater than or equal to zero, and less than the radix.
  -> a  -- ^ The given cumulative probability to map on a digit for a number system with the given radix; should be greater than or equal to zero, and less than one.
  -> Maybe Int  -- ^ The corresponding digit wrapped in a 'Just'; 'Nothing' if the given values are out of range.
cdfToNextDigit radix prefixSequence probability
  | radix > 1, probability >= 0.0, probability < 1.0, Just pre <- _fromDigits' radix prefixSequence = Just (_baseCdfToNextDigit radix (fromInteger pre * radix) probability `mod` radix)
  | otherwise = Nothing

-- | Determine the corresponding digit for a given prefix and a given cumulative probability for a decimal number system.
cdfToNextDigit10' :: (Floating a, RealFrac a)
  => Int  -- ^ The given prefix. If the value is @31@, then we thus determine the digit after @3@ and @1@. This value must be gereater than or equal to zero.
  -> a  -- ^ The given cumulative probability for which we want to retrieve the digit. Should be greater than or equal to zero and less than one.
  -> Int  -- ^ The corresponding digit for the given prefix and cumulative probability. Unspecified behavior if the given values are out of range.
cdfToNextDigit10' prefix = (`mod` 10) . _baseCdfToNextDigit 10 (10*prefix)

-- | Determine the corresponding digit for a given prefix and a given cumulative probability for a binary system.
cdfToNextDigit2' :: (Floating a, RealFrac a)
  => Int  -- ^ The given prefix. If the value is @9@, then we thus determine the digit after @1@, @0@, @0@ and @1@. This value must be greater than or equal to zero.
  -> a  -- ^ The given cumulative probability for which we want to retrieve the digit. Should be greater than or equal to zero and less than one.
  -> Int  -- ^ The corresponding digit for the given prefix and cumulative probability. Unspecified behavior if the given values are out of range.
cdfToNextDigit2' prefix = (`mod` 2) . _baseCdfToNextDigit 2 (2*prefix)

-- | Determine the corresponding digit for a given prefix and a given cumulative probability for a number system with a given radix.
cdfToNextDigit' :: (Floating a, RealFrac a)
  => Int  -- ^ The given radix, should be greater than one.
  -> [Int]  -- ^ The list of prefix digits. Leading zeros are ignored and all digits should be greater than or equal to zero and less than the given radix.
  -> a  -- ^ The cumulative probability, should be greater than or equal to zero and less than one.
  -> Int  -- ^ The corresponding digit for the given radix prefix and cumulative probability. Unspecified behavior if the given values are out of range.
cdfToNextDigit' radix ns = (`mod` radix) . _baseCdfToNextDigit radix (fromInteger (_fromDigits radix ns) * radix)

-- | A random number generator that generates the next digit after a certain prefix according to Benford's law for a decimal number system.
generateNextDigit10 :: (RandomGen g, Integral i)
  => g  -- ^ The random number generator.
  -> i  -- ^ The given prefix, should be greater than or equal to zero. Leading zeros are ignored.
  -> (Int, g)  -- ^ A 2-tuple with the next digit of a number as first item, and the modified random generator as second item.
generateNextDigit10 = generateNextDigit' 10

-- | A random number generator that generates the next digit after a certain prefix according to Benford's law for a decimal number system.
generateNextDigit2 :: (RandomGen g, Integral i)
  => g  -- ^ The random number generator.
  -> i  -- ^ The given prefix, should be greater than or equal to zero. Leading zeros are ignored.
  -> (Int, g)  -- ^ A 2-tuple with the first digit of a number as first item, and the modified random generator as second item.
generateNextDigit2 = _generatorMapping (cdfToNextDigit' @Double 2)

-- | A random number generator that generates the next digit after a certain prefix according to Benford's law for a number system with a given radix.
generateNextDigit' :: RandomGen g
  => Int  -- ^ The radix of the given number system, should be greater than one.
  -> g  -- ^ The random number generator.
  -> [Int]  -- ^ The given prefix, for @31@ for example, we generate a digit according to the Benford distribution after the @3@ and @1@ digits.
  -> (Int, g)  -- ^ A 2-tuple with the next digit of a number as first item, and the modified random generator as second item.
generateNextDigit' radix prefix = _generatorMapping (cdfToNextDigit' @Double radix prefix)

-- | A conditional random generator that generates the next digit after a certain prefix according to Benford's law for a number system with a given radix.
generateNextDigit :: RandomGen g
  => Int  -- ^ The radix of the given number system, should be greater than one.
  -> g  -- ^ The random number generator.
  -> [Int]  -- ^ The prefix of the given sequence that is used to determine the next digit.
  -> Maybe (Int, g)  -- ^ A generator for the next digit wrapped in a 'Just'; 'Nothing' if the radix is out of range.
generateNextDigit radix gen prefix
  | radix > 1 = generateNextDigit' radix gen <$> _fromDigits' radix prefix
  | otherwise = Nothing

generateBenfordSequence10 :: (Floating a, Ord a, RandomGen g)
  => a
  -> g
  -> [Int]
generateBenfordSequence10 = generateBenfordSequence 10

generateBenfordSequence2 :: (Floating a, Ord a, RandomGen g)
  => a
  -> g
  -> [Int]
generateBenfordSequence2 = generateBenfordSequence 2

generateBenfordSequence :: (Floating a, Ord a, RandomGen g)
  => Int
  -> a
  -> g
  -> [Int]
generateBenfordSequence radix = go 0
  where go = go
        -- let ~(p, g') = random g; x = nextDigitCdf radix q0 (p :: Double) in x : go  g'
--         tl = randomRs (0, radix-1)
