module CodingChallanges.LSystems where

import System.Console.ArgParser.Run (runApp)
import qualified Data.ByteString as BS (readFile)
import Data.Aeson (eitherDecodeStrict')

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Math.LinearAlgebra.Vector
import Math.LinearAlgebra.Vector.Vector2

import CodingChallanges.LSystems.Config
import CodingChallanges.LSystems.Turtle
import CodingChallanges.LSystems.LSystem
import CodingChallanges.LSystems.SVG

data State = State
    { statePaths          :: [TurtlePath]
    , statePathsGenerator :: Int -> [TurtlePath]
    , stateIterations     :: Int
    , stateVisiblePoints  :: Int
    , stateViewportSize   :: (Float, Float)
    }

pathColor :: Color
pathColor = makeColorI 255 255 255 100

padding :: Float
padding = 10

initialWindowSize :: (Int, Int)
initialWindowSize = (800, 800)

windowSizeToFloat :: (Int, Int) -> (Float, Float)
windowSizeToFloat (windowWidth, windowHeight) = (fromIntegral windowWidth, fromIntegral windowHeight)

createWindow :: (String, FilePath) -> Display
createWindow (name, file) = InWindow ("L-Systems — " ++ name ++ " (" ++ file ++ ")") initialWindowSize (50, 50)

background :: Color
background = greyN 0.2

main :: IO ()
main = do
    interface <- configParser
    runApp interface runWithConfig

runWithConfig :: Config -> IO ()
runWithConfig config = do
    system <- loadLSystem $ configFile config

    let initialState = updatePaths State
            { statePaths            = []
            , statePathsGenerator = \iterations ->
                let commands = systemTurtleCmds $ generateN iterations system
                in turtlePaths $ executeTurtleCmds commands createTurtle
            , stateIterations       = configIterations config
            , stateVisiblePoints    = 0
            , stateViewportSize     = case config of
                SVGModeConfig{} -> getSize $ configSVGSize config
                _               -> windowSizeToFloat initialWindowSize
            }

        window = createWindow (systemName system, configFile config)

    case config of
        DrawModeConfig{} -> display window background $ renderPaths $ statePaths initialState
        TurtleModeConfig{} ->
            play window background (configFPS config) initialState
                 render handleEvent (update $ configStepsPerFrame config)
        SVGModeConfig{} -> pathsToSVG config $ statePaths initialState

loadLSystem :: FilePath -> IO LSystem
loadLSystem file = do
    systemJSON <- BS.readFile file
    case eitherDecodeStrict' systemJSON of
        Right system -> return system
        Left  msg    -> error msg

updatePaths :: State -> State
updatePaths state = fitPointsInViewport $ state {statePaths = statePathsGenerator state $ stateIterations state}

fitPointsInViewport :: State -> State
fitPointsInViewport state@State { statePaths = paths
                                , stateViewportSize = (viewportWidth, viewportHeight)
                                } =
    state {statePaths = map (map (\point -> (point + translation) *^ scaleFactor )) paths}
  where
    translation     = -(size /^ 2) - leftLowerBound
    scaleFactor     = (min viewportWidth viewportHeight - padding * 2) / maxComponent size
    size            = rightUpperBound - leftLowerBound
    leftLowerBound  = foldl minComponents vZero allPoints
    rightUpperBound = foldl maxComponents vZero allPoints
    allPoints       = concat paths

render :: State -> Picture
render state = renderPaths $ takePaths (stateVisiblePoints state) $ statePaths state
  where
    takePaths :: Int -> [[Vector2 Float]] -> [[Vector2 Float]]
    takePaths _ []  = []
    takePaths n [x] = [take n x]
    takePaths n (x:xs)
        | n > len           = x : takePaths (n - len) xs
        | n <= len && n > 0 = [take n x]
        | otherwise         = []
        where len = length x

renderPaths :: [TurtlePath] -> Picture
renderPaths = color pathColor . pictures . map (line . map vectorToTuple)

handleEvent :: Event -> State -> State
handleEvent (EventKey (Char '+') Down _ _) state =
    updatePaths $ state
    { stateIterations = stateIterations state + 1
    , stateVisiblePoints = 0
    }
handleEvent (EventKey (Char '-') Down _ _) state =
    updatePaths $ state
    { stateIterations = max 0 $ stateIterations state - 1
    , stateVisiblePoints = 0
    }
handleEvent (EventResize windowSize) state = fitPointsInViewport $
    state {stateViewportSize = windowSizeToFloat windowSize}
handleEvent _ state = state

update :: Int -> Float -> State -> State
update stepsPerFrame _ state = state {stateVisiblePoints = stateVisiblePoints state + stepsPerFrame}
