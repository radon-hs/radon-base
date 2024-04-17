module Radon.Internal.Math where
  
highestPowerOfTwo :: Int -> Int
highestPowerOfTwo = helper 1
  where
    helper 4 _ = 4
    helper n x = if even x then helper (n * 2) (x `div` 2) else n