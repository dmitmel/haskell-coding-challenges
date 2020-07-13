module Math where

import Data.Fixed

ls = map ((+ 1) . (+ 2))

clamp :: Ord a => (a, a) -> a -> a
clamp (minN, maxN) n = max minN (min n maxN)

{-# INLINE clamp #-}
clamp01 :: (Ord a, Num a) => a -> a
clamp01 = clamp (0, 1)

{-# INLINE clamp01 #-}
lerp :: (Ord a, Num a) => (a, a) -> a -> a
lerp (a, b) t = a + (b - a) * clamp01 t

{-# INLINE lerp #-}
lerpUnclamped :: Num a => (a, a) -> a -> a
lerpUnclamped (a, b) t = a + (b - a) * t

{-# INLINE lerpUnclamped #-}
inverseLerp :: (Ord a, Fractional a) => (a, a) -> a -> a
inverseLerp (a, b) value
  | a /= b = clamp01 $ (value - a) / (b - a)
  | otherwise = 0

moveTowards :: (Ord a, Num a) => a -> a -> a -> a
moveTowards target maxDelta current
  | abs (target - current) <= maxDelta = target
  | otherwise = current + signum (target - current) * maxDelta

degToRad :: Floating a => a -> a
degToRad deg = deg * pi / 180

{-# INLINE degToRad #-}
radToDeg :: Floating a => a -> a
radToDeg rad = rad * 180 / pi

{-# INLINE radToDeg #-}
repeatN :: Real a => (a, a) -> a -> a
repeatN (minN, maxN) n
  | bounded < 0 = minN + bounded + maxN
  | otherwise = minN + bounded
  where
    bounded = (n - minN) `mod'` (maxN - minN)

{-# INLINE repeatN #-}
