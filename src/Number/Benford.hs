{-# LANGUAGE BangPatterns #-}

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
    -- * Reverse cumulative density function
  , cdfToFirstDigit', cdfToFirstDigit10'
    -- * Generate sequences based on Benford's law
  , generateBenfordSequence, generateBenfordSequence2, generateBenfordSequence10
  ) where

import Data.Foldable(foldl')

import System.Random(RandomGen, uniformR)

_baseFunction :: Floating a => Int -> Integer -> a
_baseFunction _ 0 = 1
_baseFunction r n = logBase (fromIntegral r) (1 + 1 / fromInteger n)

_baseFunction' :: (Floating a, Integral i) => a -> i -> a
_baseFunction' _ 0 = 1
_baseFunction' r n = logBase r (1 + 1 / fromIntegral n)

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
firstDigit radix
  | radix < 2 = const Nothing
  | otherwise = go
  where go digit
          | digit < 1 || digit >= radix = Nothing
          | otherwise = Just (firstDigit' radix digit)

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

-- | Determine the /cummulative distribution function/ for the first digit in a decimal number system.
firstDigitCdf10' :: Floating a
  => Int  -- ^ The first digit for which we determine the cummulative distribution function, the digit should be greater than zero and less than ten.
  -> a  -- ^ The probability of the digit being less than or equal to the given value. For invalid parameters, this is unspecified behavior.
firstDigitCdf10' = firstDigitCdf' 10

-- | Determine the /cummulative distribution function/ for the first digit in a number system with the given radix.
firstDigitCdf' :: Floating a
  => Int  -- ^ The given radix of the number system for which we determine the first digit.
  -> Int  -- ^ The first digit for which we determine the cummulative distribution function, the digit should be greater than zero and less than the radix.
  -> a  -- ^ The probability of the digit being less than or equal to the given value. For invalid parameters, this is unspecified behavior.
firstDigitCdf' radix digit = logBase (fromIntegral radix) (fromIntegral (digit+1))

-- | Determine the /cummulative distribution function/ for the first digit in a decimal number system.
firstDigitCdf10 :: Floating a
  => Int  -- ^ The first digit for which we determine the cummulative distribution function, the digit should be greater than zero and less than ten.
  -> Maybe a  -- ^ The probability of the digit being less than or equal to the given value wrapped in a 'Just'. If the digit is invalid, 'Nothing' is returned.
firstDigitCdf10 = firstDigitCdf 10

-- | Determine the /cummulative distribution function/ for the first digit in a number system with the given radix.
firstDigitCdf :: Floating a
  => Int  -- ^ The given radix of the number system for which we determine the first digit.
  -> Int  -- ^ The first digit for which we determine the cummulative distribution function, the digit should be greater than zero and less than the radix.
  -> Maybe a  -- ^ The probability of the digit being less than or equal to the given value wrapped in a 'Just'. If the radix or the digit is invalid, 'Nothing' is returned.
firstDigitCdf radix
  | radix < 2 = const Nothing
  | otherwise = go
  where go digit
          | digit <= 0 || digit >= radix = Nothing
          | otherwise = Just (firstDigitCdf' radix digit)

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
  -> Maybe a
startSequence2 n
  | n >= 0 = Just (startSequence2' n)
  | otherwise = Nothing

-- | Determine the probability of a number in a number system with the given radix, to start with the given sequence of digits.
-- Leading zeros are ignored and the digits are assumed to be valid digits (greater than or equal to zero, and less than the radix).
startSequence' :: Floating a
  => Int  -- ^ The given radix of the number system, should be greater than one.
  -> [Int]  -- ^ The given sequence of digits to deterime the probability for. Leading zeros are ignored, the digits should be greater than or equal to zero and less than the radix.
  -> a  -- ^ The probability of the given digit sequence in a number system with the given radix. Unspecified behavior in case the radix or digits are not valid.
startSequence' radix ns = _baseFunction radix d
  where d = foldl' go 0 ns
          where go q = (r*q +) . fromIntegral
        r = fromIntegral radix :: Integer

-- | Determine the probability of a number in a number system with the given radix to start with the given sequence of digits, leading zeros are ignored.
-- The function will return a 'Nothing' in case the radix or the digit sequence is invalid.
startSequence :: Floating a
  => Int  -- ^ The given /radix/ of the number system, must be greater than one.
  -> [Int]  -- ^  The sequence of digits in the given number system. Leading zeros are ignored, all digits must be greater than or equal to zero, and less than the radix.
  -> Maybe a  -- ^ The probability of the given digit sequence in a number system with the given radix wrapped in a 'Just'. 'Nothing' if the given radix or digit sequence is invalid.
startSequence radix
  | radix <= 1 = const Nothing
  | otherwise = go
  where go ns | Just n <- calcSum 0 ns, n >= 0 = Just (_baseFunction radix n)
              | otherwise = Nothing
        calcSum !n [] = Just n
        calcSum !n (x:xs)
          | x < 0 || x >= radix = Nothing
          | otherwise = calcSum (r*n + fromIntegral x) xs
        r = fromIntegral radix :: Integer

-- | Determine the digit for a given cumulative probability for a decimal number system.
-- The probability should be greater than or equal to zero, and less than one.
cdfToFirstDigit10' :: (Ord a, Floating a, RealFrac a)
  => a  -- ^ The given /cumulative probability/, should be greater than or equal to zero, and less than one.
  -> Int  -- ^ The smallest digit for which the cumulative probability is less than the given probability. Unspecified behavior if the radix or cumulative probability are out of range.
cdfToFirstDigit10' = cdfToFirstDigit' 10

-- | Determine the digit for a given cumulative probability for a number system with a given radix.
-- The probability should be greater than or equal to zero, and less than one.
cdfToFirstDigit' :: (Ord a, Floating a, RealFrac a)
  => Int  -- ^ The given /radix/, should be greater than one.
  -> a  -- ^ The given /cumulative probability/, should be greater than or equal to zero, and less than one.
  -> Int  -- ^ The smallest digit for which the cumulative probability is less than the given probability. Unspecified behavior if the radix or cumulative probability are out of range.
cdfToFirstDigit' radix cprob = floor (fromIntegral radix ** cprob)

_reverseLookup :: (Floating a, Ord a) => a -> Int -> Int -> Int -> Int
_reverseLookup prob radix mn mx = _reverseLookup' prob radix mn mn mx

_reverseLookup' :: (Floating a, Ord a) => a -> Int -> Int -> Int -> Int -> Int
_reverseLookup' prob radix mn = go
  where r = fromIntegral radix
        go p0 p2
          | p2 <= p0 = p0
          | fromIntegral p1' / fromIntegral mn < r ** prob = go p1' p2
          | otherwise = go p0 p1
          where p1 = div (p0 + p2) 2
                p1' = p1 + 1

generateBenfordSequence10 :: RandomGen g
  => g
  -> [Int]
generateBenfordSequence10 = generateBenfordSequence 10

generateBenfordSequence2 :: RandomGen g
  => g
  -> [Int]
generateBenfordSequence2 = generateBenfordSequence 2

generateBenfordSequence :: RandomGen g
  => Int
  -> g
  -> [Int]
generateBenfordSequence radix = go 1
    where go q0 g = let ~(p, g') = uniformR (0, 1) g; x =_reverseLookup' (p :: Double) radix q0 q0 (q0+radix-1) in x : go (x*radix) g'
