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
    -- * Probabilities of numbers starting with a sequence of digits
  , startSequence', startSequence2', startSequence10'
  , startSequence, startSequence2, startSequence10
  ) where

import Data.Foldable(foldl')

-- import System.Random(Random, RandomGen)

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

-- | Determine the probability of the sequence starting with the given digits. The leading zeros are ignored. If you
-- thus call @startSequence10' 1425@, you obtain the probability of a number starting with @1@, @4@, @2@, and @5@ as digits.
startSequence10' :: (Integral i, Floating a)
  => i  -- ^ The 'Integral' number that contains the start sequence (with an optional sequence of "leading zeros"), should be greater than zero.
  -> a  -- ^ The probability of a number to start with the given start sequence. Unspecified if the number is less than zero.
startSequence10' 0 = 1
startSequence10' n = logBase 10 (1 + 1 / n')
  where n' = fromIntegral n

-- | Determine the probability of the sequence starting with the given digits of a binary number. Binary numbers always start
-- with @1@. If you thus call @startSequence2' 9@, you get the probability of a binary number starting with @1@, @0@, @0@, and @1@
-- as binary digits.
startSequence2' :: (Integral i, Floating a)
  => i  -- ^ The 'Integral' binary number that contains the binary start sequence (with an optional sequence of "leading zeros"), should be greater than zero.
  -> a  -- ^ The probability of a binary number starting with the givenn binary sequence. Unspecified if the number is less than zero.
startSequence2' 0 = 1
startSequence2' n = logBase 2 (1 + 1 / fromIntegral n)

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
startSequence' radix ns
  | d == 0 = 1
  | otherwise = logBase (fromIntegral radix) (1 + 1 / fromIntegral d)
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
  where go _ = Nothing

{-
generateBenfordSequence10 :: RandomGen g
  => g
  -> [Int]
generateBenfordSequence10 = generateBenfordSequence 10

generateBenfordSequence2 :: RandomGen g
  => g
  -> [Int]
generateBenfordSequence2 = generateBenfordSequence 2

generateBenfordSequence :: (Integral a, Random a, RandomGen g)
  => Int
  -> g
  -> [a]
generateBenfordSequence =
-}
