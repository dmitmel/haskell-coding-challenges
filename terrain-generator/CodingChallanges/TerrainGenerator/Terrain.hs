{-|
Module      : CodingChallanges.TerrainGenerator.Terrain
Copyright   : (c) 2017 Dmytro Meleshko
License     : Apache-2.0
Maintainer  : dmytro.meleshko@gmail.com
Stability   : stable
-}

module CodingChallanges.TerrainGenerator.Terrain where

import Graphics.Gloss
import CodingChallanges.TerrainGenerator.SimplexNoise

-- | The 'Terrain' type is a list of 'Block's.
data Terrain = Terrain
    { terrainWidth  :: Int
    , terrainHeight :: Int
    , terrainBlocks :: [Block]
    } deriving (Read, Eq, Ord, Show)

-- | The 'Block' type is 3D vector. X and Y components are positions on 'Terrain',
-- Z component is height of this block.
type Block = (Float, Float, Float)

-- | The 'generateTerrain' function takes terrain parameters and generates terrain
-- using the simplex noise. See 'CodingChallanges.TerrainGenerator.SimplexNoise.noise2D'.
generateTerrain
    :: (Int, Int)   -- ^ size of terrain
    -> Float        -- ^ noise scale
    -> Int          -- ^ octaves (iterations for generating each block)
    -> Float        -- ^ persistance (how many changes each octave makes)
    -> Float        -- ^ lacunarity (how much changes in octave affect block)
    -> Point        -- ^ offset
    -> Terrain
generateTerrain (width, height) noiseScale octaves persistance lacunarity (offsetX, offsetY) =
    let centerX = fromIntegral width / 2
        centerY = fromIntegral height / 2
        blocks =
            [ generateBlock (fromIntegral x - centerX, fromIntegral y - centerY)
            | y <- [0..height - 1]
            , x <- [0..width - 1]
            ]
    in Terrain width height blocks
  where
    generateBlock :: Point -> Block
    generateBlock (x, y) =
        (x, y, sum (take octaves $ generateOctaves 1 1 (x, y)) / fromIntegral octaves)

    generateOctaves :: Float -> Float -> Point -> [Float]
    generateOctaves amplitude frequency (x, y) =
        generateOctave amplitude frequency (x, y) :
        generateOctaves (amplitude * persistance) (lacunarity * lacunarity) (x, y)

    generateOctave :: Float -> Float -> Point -> Float
    generateOctave amplitude frequency (x, y) =
        let sampleX = (x / noiseScale + offsetX) * frequency
            sampleY = (y / noiseScale + offsetY) * frequency
        in noise2D sampleX sampleY * amplitude + 1 / 2

-- | The 'Region' type represents region of 'Terrain'. Here're some examples of
-- regions:
--
-- 1. Water
-- 2. Sand
-- 3. Grass
-- 4. Mountains
-- 5. Snow
data Region = Region
    { regionMaxHeight :: Float
    , regionColor     :: Color
    } deriving (Eq, Show)

-- | The 'blockColor' function takes list of 'Region's and 'Block's, and retuns
-- 'Color' for this block depending on its Z value.
blockColor :: [Region] -> Block -> Color
blockColor []                  _ = black
blockColor [Region _ regColor] _ = regColor
blockColor (Region maxHeight regColor:regions) block@(_, _, z)
    | z <= maxHeight = regColor
    | otherwise = blockColor regions block
