{-|
Module      : Main
Description : 2D Terrain Generator
Copyright   : (c) 2017 Dmytro Meleshko
License     : Apache-2.0
Maintainer  : dmytro.meleshko@gmail.com
Stability   : stable

= 2D Terrain Generator (<https://youtu.be/IKB1hWWedMk?list=PLRqwX-V7Uu6ZiZxtDDRCi6uhfTH4FilpH>)

@
cabal run terrain-generator
@

== Controls

[Zoom]: W/S key
[Move the camera]: left-drag
[Reset]: R key

== See Also

<https://youtu.be/wbpMiKiSKm8?list=PLFt_AvWsXl0eBW2EiBtl_sxmDtSgZBxB3 Procedural Landmass Generation> -
a great explanation of procedural 3D terrain generation in <https://unity3d.com Unity>.
-}

module CodingChallanges.TerrainGenerator where

import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Raster.Pixels
import CodingChallanges.TerrainGenerator.Terrain
import Math.LinearAlgebra.Vector
import Math.LinearAlgebra.Vector.Vector2

-- | The 'State' type stores application data for rendering, event handling and
-- updating 'Terrain'.
data State = State
    { stateTerrain    :: Maybe Terrain   -- ^ Current terrain. It's stored here because
                                         -- it is generated only when user moves the camera
                                         -- or changes zoom. Also, when user makes these
                                         -- actions, terrain becomes less detailed.
    , stateOffset     :: Point           -- ^ Current terrain offset. It changes when user
                                         -- moves the camera.
    , stateNoiseScale :: Float           -- ^ Current terrain noise scale. It changes when
                                         -- user changes zoom.
    , stateBlockSize  :: Float           -- ^ Current block size.
    , stateWindowSize :: (Int, Int)      -- ^ Current size of window. Terrain size is
                                         -- calculated on each 'updateTerrain' call,
                                         -- so terrain changes with the window size.
    , stateZooming    :: Maybe Float     -- ^ Contains zooming increment if user is changing
                                         -- zoom.
    , stateDragging   :: Maybe Point     -- ^ Contains previous mouse position if user is
                                         -- moving the camera.
    } deriving (Read, Eq, Ord, Show)

initialWindowSize :: (Int, Int)
initialWindowSize = (600, 600)

fps :: Int
fps = 20

-- | The 'highResBlockSize' constant contains size of block for high resolution
-- terrain. It's used when user doesn't interact with terrain, because it takes
-- more time to generate high resolution terrain.
highResBlockSize :: Float
highResBlockSize = 3

-- | The 'lowResBlockSize' constant contains size of block for low resolution
-- terrain. It's used when user interacts with terrain to increase performace.
lowResBlockSize :: Float
lowResBlockSize = 10

compassLength :: Float
compassLength = 50

window :: Display
window = InWindow "Terrain Generator" initialWindowSize (50, 50)

background :: Color
background = black

defaultNoiseScale :: Float
defaultNoiseScale = 100

zoomSensitivity :: Float
zoomSensitivity = 0.2

defaultOctaves :: Int
defaultOctaves = 3

defaultPersistance :: Float
defaultPersistance = 0.5

defaultLacunarity :: Float
defaultLacunarity = 2

regions :: [Region]
regions =
    [ Region 0.3  $ makeColor 0.25  0.477 0.808 1   -- Water Deep
    , Region 0.4  $ makeColor 0.268 0.492 0.821 1   -- Water Shallow
    , Region 0.45 $ makeColor 0.856 0.842 0.562 1   -- Sand
    , Region 0.55 $ makeColor 0.403 0.647 0.116 1   -- Grass
    , Region 0.6  $ makeColor 0.301 0.484 0.086 1   -- Grass 2
    , Region 0.7  $ makeColor 0.429 0.343 0.302 1   -- Rock
    , Region 0.9  $ makeColor 0.367 0.301 0.271 1   -- Rock 2
    , Region 1    $ makeColor 0.999 1     1     1   -- Snow
    ]

main :: IO ()
main = play window background fps initialState render handleEvent update

initialState :: State
initialState =
    State
    { stateTerrain    = Nothing
    , stateOffset     = (0, 0)
    , stateNoiseScale = defaultNoiseScale
    , stateBlockSize  = highResBlockSize
    , stateWindowSize = initialWindowSize
    , stateZooming    = Nothing
    , stateDragging   = Nothing
    }

-- | The 'calculateTerrainSize' calculates terrain size using block size and window
-- size.
calculateTerrainSize
    :: Float        -- ^ block size
    -> (Int, Int)   -- ^ window size
    -> (Int, Int)
calculateTerrainSize blockSize (windowWidth, windowHeight) =
    (floor $ fromIntegral windowWidth / blockSize, floor $ fromIntegral windowHeight / blockSize)
{-# INLINE calculateTerrainSize #-}

render :: State -> Picture
render State {stateTerrain = Just (Terrain width height blocks), stateBlockSize = blockSize, stateOffset = (offsetX, offsetY), stateNoiseScale = noiseScale} =
    pictures
    [ scale blockSize blockSize $
      makePicture BottomToTop (width, height) $
      map (blockColor regions) blocks
    , color red $ line arrow
    ]
  where
    arrow = map (vectorToTuple . (*^ compassLength) . applyAxes (xAxis, yAxis))
        [ V2 0.5 0, V2 0.15   0.35      -- left
        , V2 0.5 0, V2 (-0.5) 0         -- middle
        , V2 0.5 0, V2 0.15   (-0.35)   -- right
        ]
    xAxis = normalize $ V2 (-offsetX * noiseScale) (-offsetY * noiseScale)
    yAxis = perpindicularR xAxis
render _ = blank

handleEvent :: Event -> State -> State
handleEvent event state
    | EventKey (MouseButton LeftButton) Down _ mousePos <- event
    = updateTerrain $ state {stateBlockSize = lowResBlockSize, stateDragging = Just mousePos}
    | EventKey (MouseButton LeftButton) Up _ _ <- event
    = updateTerrain $ state {stateBlockSize = highResBlockSize, stateDragging = Nothing}
    | EventMotion (mouseX, mouseY)  <- event
    , (offsetX, offsetY)            <- stateOffset state
    , noiseScale                    <- stateNoiseScale state
    , Just (prevMouseX, prevMouseY) <- stateDragging state
    = updateTerrain $
        state
        { stateOffset =
            ( offsetX + (prevMouseX - mouseX) / noiseScale
            , offsetY + (prevMouseY - mouseY) / noiseScale)
        , stateBlockSize = lowResBlockSize
        , stateDragging  = Just (mouseX, mouseY)
        }
    | EventKey (Char 'w') Down _ _ <- event
    = state {stateZooming = Just (1 + zoomSensitivity)}
    | EventKey (Char 's') Down _ _ <- event
    = state {stateZooming = Just (1 - zoomSensitivity)}
    | EventKey _ Up _ _ <- event
    = updateTerrain $ state {stateBlockSize = highResBlockSize, stateZooming = Nothing}
    | EventKey (Char 'r') Down _ _ <- event
    = updateTerrain initialState
    -- EventResize is fired when window appears, so terrain must be in high resolution
    -- on the first event and in low on next events
    | EventResize windowSize <- event
    , terrain                <- stateTerrain state
    = let blockSize = if isJust terrain then lowResBlockSize else highResBlockSize
      in updateTerrain $ state {stateBlockSize = blockSize, stateWindowSize = windowSize}
    | otherwise = state

update :: Float -> State -> State
update _ state@State {stateNoiseScale = noiseScale, stateZooming = Just zoom} =
    -- If zoom changes, terrain will be updated
    updateTerrain $ state {stateNoiseScale = noiseScale * zoom, stateBlockSize = lowResBlockSize}
update _ state = state

-- | The 'updateTerrain' function generates new terrain using values in the 'State'.
updateTerrain :: State -> State
updateTerrain state@State { stateOffset     = offset
                          , stateNoiseScale = noiseScale
                          , stateBlockSize  = blockSize
                          , stateWindowSize = windowSize
                          } =
    state
    { stateTerrain = Just $ generateTerrain
        (calculateTerrainSize blockSize windowSize)
        (noiseScale / blockSize)     -- Noise scale must be divided by block size to make
                                     -- zoom identical for different block sizes
        defaultOctaves defaultPersistance defaultLacunarity offset
    }
{-# INLINE updateTerrain #-}
