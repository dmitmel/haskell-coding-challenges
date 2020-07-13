module Math.LinearAlgebra.Vector.Vector3 where

import Math
import System.Random
import Math.LinearAlgebra.Vector
import Math.LinearAlgebra.Epsilon

data Vector3 a = V3 a a a deriving (Eq, Ord)

vBack, vDown, vForward, vLeft, vOne, vRight, vUp, vZero :: Num a => Vector3 a
vBack    = V3 0    0    (-1)
vDown    = V3 0    (-1) 0
vForward = V3 0    0    1
vLeft    = V3 (-1) 0    0
vOne     = V3 1    1    1
vRight   = V3 1    0    0
vUp      = V3 0    1    0
vZero    = V3 0    0    0

vectorToTuple :: Vector3 a -> (a, a, a)
vectorToTuple (V3 x y z) = (x, y, z)

tupleToVector :: (a, a, a) -> Vector3 a
tupleToVector (x, y, z) = V3 x y z

instance Show a => Show (Vector3 a) where
    show (V3 x y z) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

instance Read a => Read (Vector3 a) where
    readsPrec _ s1 =
         [ (V3 x y z, s8)
         | ("(", s2) <- lex s1
         , (x,   s3) <- reads s2
         , (",", s4) <- lex s3
         , (y,   s5) <- reads s4
         , (",", s6) <- lex s5
         , (z,   s7) <- reads s6
         , (")", s8) <- lex s7
         ]

instance Enum a => Enum (Vector3 a) where
    succ (V3 x y z) = V3 (succ x) (succ y) (succ z)
    {-# INLINE succ #-}
    pred (V3 x y z) = V3 (pred x) (pred y) (pred z)
    {-# INLINE pred #-}

    toEnum n = V3 (toEnum n) (toEnum n) (toEnum n)
    {-# INLINE toEnum #-}
    fromEnum (V3 x y z)
        | nx == ny && ny == nz && nz == nx = nx
        | otherwise                        = undefined
      where
        nx = fromEnum x
        ny = fromEnum y
        nz = fromEnum z

    enumFrom (V3 nx ny nz) = zipWith3 V3 [nx..] [ny..] [nz..]
    {-# INLINE enumFrom #-}
    enumFromThen (V3 nx ny nz) (V3 x' y' z') = zipWith3 V3 [nx,x'..] [ny,y'..] [nz,z'..]
    {-# INLINE enumFromThen #-}
    enumFromTo (V3 nx ny nz) (V3 mx my mz) = zipWith3 V3 [nx..mx] [ny..my] [nz..mz]
    {-# INLINE enumFromTo #-}
    enumFromThenTo (V3 nx ny nz) (V3 x' y' z') (V3 mx my mz) = zipWith3 V3 [nx,x'..mx] [ny,y'..my] [nz,z',mz]
    {-# INLINE enumFromThenTo #-}

instance Functor Vector3 where
    fmap f (V3 x y z) = V3 (f x) (f y) (f z)
    {-# INLINE fmap #-}

instance Num a => Num (Vector3 a) where
    (V3 x1 y1 z1) + (V3 x2 y2 z2) = V3 (x1 + x2) (y1 + y2) (z1 + z2)
    {-# INLINE (+) #-}
    (V3 x1 y1 z1) - (V3 x2 y2 z2) = V3 (x1 - x2) (y1 - y2) (z1 - z2)
    {-# INLINE (-) #-}
    (V3 x1 y1 z1) * (V3 x2 y2 z2) = V3 (x1 * x2) (y1 * y2) (z1 * z2)
    {-# INLINE (*) #-}

    negate (V3 x y z) = V3 (-x) (-y) (-z)
    {-# INLINE negate #-}
    abs (V3 x y z) = V3 (abs x) (abs y) (abs z)
    {-# INLINE abs #-}
    signum (V3 x y z) = V3 (signum x) (signum y) (signum z)
    {-# INLINE signum #-}
    fromInteger n = V3 (fromInteger n) (fromInteger n) (fromInteger n)
    {-# INLINE fromInteger #-}

instance Fractional a => Fractional (Vector3 a) where
    (V3 x1 y1 z1) / (V3 x2 y2 z2) = V3 (x1 / x2) (y1 / y2) (z1 / z2)
    {-# INLINE (/) #-}
    recip (V3 x y z) = V3 (recip x) (recip y) (recip z)
    {-# INLINE recip #-}
    fromRational n = V3 (fromRational n) (fromRational n) (fromRational n)
    {-# INLINE fromRational #-}

instance Vector Vector3 where
    (V3 x y z) *^ s = V3 (x * s) (y * s) (z * s)
    {-# INLINE (*^) #-}
    s ^* (V3 x y z) = V3 (s * x) (s * y) (s * z)
    {-# INLINE (^*) #-}
    (V3 x y z) /^ s = V3 (x / s) (y / s) (z / s)
    {-# INLINE (/^) #-}
    s ^/ (V3 x y z) = V3 (s / x) (s / y) (s / z)
    {-# INLINE (^/) #-}

    magnitude (V3 x y z) = sqrt $ x * x + y * y + z * z
    {-# INLINE magnitude #-}
    sqrMagnitude (V3 x y z) = x * x + y * y + z * z
    {-# INLINE sqrMagnitude #-}
    normalize v
        | nearZero m = vZero
        | otherwise  = v /^ m
        where m = magnitude v
    {-# INLINE normalize #-}

    from `distance` to = magnitude $ from - to
    {-# INLINE distance #-}
    from `sqrDistance` to = sqrMagnitude $ from - to
    {-# INLINE sqrDistance #-}
    from `direction` to = normalize $ to - from
    {-# INLINE direction #-}

    (V3 x1 y1 z1) `dot` (V3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2
    {-# INLINE dot #-}

    minComponents (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (min x1 x2) (min y1 y2) (min z1 z2)
    {-# INLINE minComponents #-}
    minComponent (V3 x y z) = min (min x y) z
    {-# INLINE minComponent #-}
    maxComponents (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (max x1 x2) (max y1 y2) (max z1 z2)
    {-# INLINE maxComponents #-}
    maxComponent (V3 x y z) = max (max x y) z
    {-# INLINE maxComponent #-}

    lerp (a, b) t = a + (b - a) *^ clamp01 t
    {-# INLINE lerp #-}
    lerpUnclamped (a, b) t = a + (b - a) *^ t
    {-# INLINE lerpUnclamped #-}
    moveTowards target maxDistanceDelta current
        | m <= maxDistanceDelta = target
        | otherwise             = current + difference /^ (m * maxDistanceDelta)
      where
        difference = target - current
        m = magnitude difference
    repeatV (V3 nx ny nz, V3 mx my mz) (V3 x y z) = V3 (repeatN (nx, mx) x) (repeatN (ny, my) y) (repeatN (nz, mz) z)
    {-# INLINE repeatV #-}

    reflect normal dir = (-2 * (normal `dot` dir)) ^* normal + dir
    {-# INLINE reflect #-}

cross :: Num a => Vector3 a -> Vector3 a -> Vector3 a
cross (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

applyAxes :: Num a => (Vector3 a, Vector3 a, Vector3 a) -> Vector3 a -> Vector3 a
applyAxes (V3 a b c, V3 d e f, V3 g h i) (V3 x y z) = V3 (x*a + y*d + z*g) (x*b + y*e + z*h) (x*c + y*f + z*i)

instance Random a => Random (Vector3 a) where
    random gen =
        let (x, gen2) = random gen
            (y, gen3) = random gen2
            (z, gen4) = random gen3
        in (V3 x y z, gen4)
    {-# INLINE random #-}

    randomR (V3 nx ny nz, V3 mx my mz) gen =
        let (x, gen2) = randomR (nx, mx) gen
            (y, gen3) = randomR (ny, my) gen2
            (z, gen4) = randomR (nz, mz) gen3
        in (V3 x y z, gen4)
    {-# INLINE randomR #-}
