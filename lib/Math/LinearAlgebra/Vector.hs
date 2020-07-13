module Math.LinearAlgebra.Vector where

import Math.LinearAlgebra.Epsilon
import Math

infixl 7 *^, ^*, /^

class Vector v where
    (*^) :: Num a => v a -> a -> v a
    (^*) :: Num a => a -> v a -> v a
    (/^) :: Fractional a => v a -> a -> v a
    (^/) :: Fractional a => a -> v a -> v a

    magnitude :: Floating a => v a -> a
    sqrMagnitude :: Num a => v a -> a
    normalize :: (Epsilon a, Floating a) => v a -> v a

    clampMagnitude :: (Ord a, Epsilon a, Floating a) => a -> v a -> v a
    clampMagnitude maxM v
        | sqrMagnitude v > maxM * maxM = normalize v *^ maxM
        | otherwise = v

    distance :: Floating a => v a -> v a -> a
    sqrDistance :: Floating a => v a -> v a -> a
    direction :: (Epsilon a, Floating a) => v a -> v a -> v a

    dot :: Num a => v a -> v a -> a

    minComponents :: Ord a => v a -> v a -> v a
    minComponent  :: Ord a => v a -> a
    maxComponents :: Ord a => v a -> v a -> v a
    maxComponent  :: Ord a => v a -> a

    lerp :: (Ord a, Num a) => (v a, v a) -> a -> v a
    lerpUnclamped :: (Ord a, Num a) => (v a, v a) -> a -> v a
    moveTowards :: (Ord a, Floating a) => v a -> a -> v a -> v a
    repeatV :: Real a => (v a, v a) -> v a -> v a

    reflect :: Num a => v a -> v a -> v a

    angleDeg, angleRad :: (Ord a, Epsilon a, Floating a) => v a -> v a -> a
    from `angleDeg` to = radToDeg $ from `angleRad` to
    {-# INLINE angleDeg #-}
    from `angleRad` to = acos $ clamp (-1, 1) $ normalize from `dot` normalize to
    {-# INLINE angleRad #-}
