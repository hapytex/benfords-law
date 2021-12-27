module Number.Benford (
    firstDigit, firstDigit10
  , firstDigit', firstDigit10'
  , startSequence', startSequence2', startSequence10'
  , startSequence10
  ) where

import Data.Foldable(foldl')

firstDigit10 :: Floating a => Int -> Maybe a
firstDigit10 = firstDigit 10

firstDigit :: Floating a => Int -> Int -> Maybe a
firstDigit radix digit
  | digit < 1 || digit >= radix = Nothing
  | otherwise = Just (firstDigit' radix digit)

firstDigit10' :: Floating a => Int -> a
firstDigit10' = firstDigit' 10

firstDigit' :: Floating a => Int -> Int -> a
firstDigit' radix digit = logBase (fromIntegral radix) ((d + 1) / d)
    where d = fromIntegral digit

startSequence10' :: Floating a => Int -> a
startSequence10' n = logBase 10 (1 + 1 / n')
  where n' = fromIntegral n

startSequence2' :: Floating a => Int -> a
startSequence2' n = logBase 2 (1 + 1 / fromIntegral n)

startSequence10 :: Floating a => Int -> Maybe a
startSequence10 n
  | n > 0 = Just (startSequence10' n)
  | otherwise = Nothing

startSequence' :: Floating a => Int -> [Int] -> a
startSequence' radix ns = logBase (fromIntegral radix) (1 + 1 / fromIntegral d)
  where d = foldl' go 0 ns
          where go q = (r*q +) . (fromIntegral :: Int -> Integer)
        r = fromIntegral radix
