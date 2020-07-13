module Math.LinearAlgebra.Vector.Vector2 where

import Math
import System.Random
import Math.LinearAlgebra.Vector
import Math.LinearAlgebra.Epsilon

data Vector2 a = V2 a a deriving (Eq, Ord)

vDown, vLeft, vOne, vRight, vUp, vZero :: Num a => Vector2 a
vDown  = V2 0    (-1)
vLeft  = V2 (-1) 0
vOne   = V2 1    1
vRight = V2 1    0
vUp    = V2 0    1
vZero  = V2 0    0

vectorToTuple :: Vector2 a -> (a, a)
vectorToTuple (V2 x y) = (x, y)

tupleToVector :: (a, a) -> Vector2 a
tupleToVector (x, y) = V2 x y

instance Show a => Show (Vector2 a) where
    show (V2 x y) = "(" ++ show x ++ "," ++ show y ++ ")"

instance Read a => Read (Vector2 a) where
    readsPrec _ s1 =
         [ (V2 x y, s6)
         | ("(", s2) <- lex s1
         , (x,   s3) <- reads s2
         , (",", s4) <- lex s3
         , (y,   s5) <- reads s4
         , (")", s6) <- lex s5
         ]

instance Enum a => Enum (Vector2 a) where
    succ (V2 x y) = V2 (succ x) (succ y)
    {-# INLINE succ #-}
    pred (V2 x y) = V2 (pred x) (pred y)
    {-# INLINE pred #-}

    toEnum n = V2 (toEnum n) (toEnum n)
    {-# INLINE toEnum #-}
    fromEnum (V2 x y)
        | nx == ny  = nx
        | otherwise = undefined
      where
        nx = fromEnum x
        ny = fromEnum y

    enumFrom (V2 nx ny) = zipWith V2 [nx..] [ny..]
    {-# INLINE enumFrom #-}
    enumFromThen (V2 nx ny) (V2 x' y') = zipWith V2 [nx,x'..] [ny,y'..]
    {-# INLINE enumFromThen #-}
    enumFromTo (V2 nx ny) (V2 mx my) = zipWith V2 [nx..mx] [ny..my]
    {-# INLINE enumFromTo #-}
    enumFromThenTo (V2 nx ny) (V2 x' y') (V2 mx my) = zipWith V2 [nx,x'..mx] [ny,y'..my]
    {-# INLINE enumFromThenTo #-}

instance Functor Vector2 where
    fmap f (V2 x y) = V2 (f x) (f y)
    {-# INLINE fmap #-}

instance Num a => Num (Vector2 a) where
    (V2 x1 y1) + (V2 x2 y2) = V2 (x1 + x2) (y1 + y2)
    {-# INLINE (+) #-}
    (V2 x1 y1) - (V2 x2 y2) = V2 (x1 - x2) (y1 - y2)
    {-# INLINE (-) #-}
    (V2 x1 y1) * (V2 x2 y2) = V2 (x1 * x2) (y1 * y2)
    {-# INLINE (*) #-}

    negate (V2 x y) = V2 (-x) (-y)
    {-# INLINE negate #-}
    abs (V2 x y) = V2 (abs x) (abs y)
    {-# INLINE abs #-}
    signum (V2 x y) = V2 (signum x) (signum y)
    {-# INLINE signum #-}
    fromInteger n = V2 (fromInteger n) (fromInteger n)
    {-# INLINE fromInteger #-}

instance Fractional a => Fractional (Vector2 a) where
    (V2 x1 y1) / (V2 x2 y2) = V2 (x1 / x2) (y1 / y2)
    {-# INLINE (/) #-}
    recip (V2 x y) = V2 (recip x) (recip y)
    {-# INLINE recip #-}
    fromRational n = V2 (fromRational n) (fromRational n)
    {-# INLINE fromRational #-}

instance Vector Vector2 where
    (V2 x y) *^ s = V2 (x * s) (y * s)
    {-# INLINE (*^) #-}
    s ^* (V2 x y) = V2 (s * x) (s * y)
    {-# INLINE (^*) #-}
    (V2 x y) /^ s = V2 (x / s) (y / s)
    {-# INLINE (/^) #-}
    s ^/ (V2 x y) = V2 (s / x) (s / y)
    {-# INLINE (^/) #-}

    magnitude (V2 x y) = sqrt $ x * x + y * y
    {-# INLINE magnitude #-}
    sqrMagnitude (V2 x y) = x * x + y * y
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

    (V2 x1 y1) `dot` (V2 x2 y2) = x1 * x2 + y1 * y2
    {-# INLINE dot #-}

    minComponents (V2 x1 y1) (V2 x2 y2) = V2 (min x1 x2) (min y1 y2)
    {-# INLINE minComponents #-}
    minComponent (V2 x y) = min x y
    {-# INLINE minComponent #-}
    maxComponents (V2 x1 y1) (V2 x2 y2) = V2 (max x1 x2) (max y1 y2)
    {-# INLINE maxComponents #-}
    maxComponent (V2 x y) = max x y
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
    repeatV (V2 nx ny, V2 mx my) (V2 x y) = V2 (repeatN (nx, mx) x) (repeatN (ny, my) y)
    {-# INLINE repeatV #-}

    reflect normal dir = (-2 * (normal `dot` dir)) ^* normal + dir
    {-# INLINE reflect #-}

rotateDeg, rotateRad :: Floating a => a -> Vector2 a -> Vector2 a
rotateDeg deg = rotateRad (degToRad deg)
{-# INLINE rotateDeg #-}
rotateRad rad (V2 x y) = V2 (ca * x - sa * y) (sa * x + ca * y)
  where
    ca = cos rad
    sa = sin rad
{-# INLINE rotateRad #-}

rotateAroundDeg, rotateAroundRad :: Floating a => Vector2 a -> a -> Vector2 a -> Vector2 a
rotateAroundDeg pivot deg = rotateAroundRad pivot (degToRad deg)
{-# INLINE rotateAroundDeg #-}
rotateAroundRad (V2 px py) rad (V2 x y) = V2 (ca*rx - sa*ry + px) (sa*rx + ca*ry + py)
  where
    rx = x - px
    ry = y - py
    ca = cos rad
    sa = sin rad
{-# INLINE rotateAroundRad #-}

perpindicularL, perpindicularR :: Num a => Vector2 a -> Vector2 a
perpindicularL (V2 x y) = V2 (-y) x
{-# INLINE perpindicularL #-}
perpindicularR (V2 x y) = V2 y (-x)
{-# INLINE perpindicularR #-}

applyAxes :: Num a => (Vector2 a, Vector2 a) -> Vector2 a -> Vector2 a
applyAxes (V2 a b, V2 c d) (V2 x y) = V2 (x*a + y*c) (x*b + y*d)
{-# INLINE applyAxes #-}

instance Random a => Random (Vector2 a) where
    random gen =
        let (x, gen2) = random gen
            (y, gen3) = random gen2
        in (V2 x y, gen3)
    {-# INLINE random #-}

    randomR (V2 nx ny, V2 mx my) gen =
        let (x, gen2) = randomR (nx, mx) gen
            (y, gen3) = randomR (ny, my) gen2
        in (V2 x y, gen3)
    {-# INLINE randomR #-}

randomOnUnitCircle :: (RandomGen g, Random a, Floating a) => g -> (Vector2 a, g)
randomOnUnitCircle rndGen =
    let (angle, rndGen2) = randomR (0, 2*pi) rndGen
    in (rotateRad angle vUp, rndGen2)
{-# INLINE randomOnUnitCircle #-}

randomInUnitCircle :: (RandomGen g, Random a, Floating a) => g -> (Vector2 a, g)
randomInUnitCircle rndGen =
    let (onUnitCircle, rndGen2) = randomOnUnitCircle rndGen
        (len, rndGen3)          = randomR (0, 1) rndGen2
    in (onUnitCircle *^ len, rndGen3)
{-# INLINE randomInUnitCircle #-}
