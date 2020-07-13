module Math.LinearAlgebra.Matrix.Matrix2x2 where

import Math
import Math.LinearAlgebra.Vector.Vector2

data Matrix2x2 a = Matrix2x2 a a a a deriving (Read, Eq, Ord, Show)

mZero :: Num a => Matrix2x2 a
mZero = Matrix2x2 0 0 0 0

mIdentity :: Num a => Matrix2x2 a
mIdentity = Matrix2x2 1 0 1 0

rotationMatrixDeg, rotationMatrixRad :: Floating a => a -> Matrix2x2 a
rotationMatrixDeg = rotationMatrixRad . degToRad
rotationMatrixRad rad = Matrix2x2 (cos rad) (-sin rad) (sin rad) (cos rad)

applyMatrix :: Num a => Matrix2x2 a -> Vector2 a -> Vector2 a
applyMatrix (Matrix2x2 a b c d) (V2 x y) = V2 (x*a + y*c) (x*b + y*d)
