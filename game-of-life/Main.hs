{-|
Module      : Main
Description : Game of Life
Copyright   : (c) 2017 Dmytro Meleshko
License     : Apache-2.0
Maintainer  : dmytro.meleshko@gmail.com
Stability   : stable

= Implementation of Game of Life in Haskell (<https://youtu.be/tENSCEO-LEc?list=PLRqwX-V7Uu6YrWXvEQFOGbCt6cX83Xunm>)

@
cabal run game-of-life
@

== Controls

[Zoom]: control-left-click drag or right-click
[Move the camera]: arrows
[Add/Remove cells]: left-click on a cell
[Play/Pause simulation]: space-key
[Clear grid]: \'C'-key

== #Rules# <https://wikipedia.org/wiki/Game_of_Life#Rules Rules> of <https://wikipedia.org/wiki/Game_of_Life Game of Life>
/Thanks to Wikipedia for the following content. See also/
<https://wikipedia.org/wiki/Game_of_Life#Examples_of_patterns Examples of patterns>

The universe of the Game of Life is an infinite two-dimensional orthogonal grid
of square cells, each of which is in one of two possible states, alive or dead,
or "populated" or "unpopulated". Every cell interacts with its eight neighbours,
which are the cells that are horizontally, vertically, or diagonally adjacent.
At each step in time, the following transitions occur:

1. Any live cell with fewer than two live neighbours dies, as if caused by
   underpopulation.
2. Any live cell with two or three live neighbours lives on to the next generation.
3. Any live cell with more than three live neighbours dies, as if by overpopulation.
4. Any dead cell with exactly three live neighbours becomes a live cell, as if by
   reproduction.

The initial pattern constitutes the seed of the system. The first generation is
created by applying the above rules simultaneously to every cell in the seed—births
and deaths occur simultaneously, and the discrete moment at which this happens is
sometimes called a tick (in other words, each generation is a pure function of the
preceding one). The rules continue to be applied repeatedly to create further
generations.
-}

module Main where

import Data.List
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- | The 'State' type is a current state of application. It stores application
-- data for rendering, event handling and updating 'Generation'.
data State
    = State
    { stateGeneration   :: Generation   -- ^ Current generation.
    , stateRunning      :: Bool         -- ^ Toggles generation updating. Rate
                                        -- of updates is configured by the 'fps'
                                        -- constant. Use space-key to toggle this
                                        -- variable.
    , stateMouseMode    :: MouseMode    -- ^ Current mouse mode.
    , statePrevMousePos :: Point        -- ^ Previous mouse position for detecting
                                        -- zooming.
    , stateWindowSize   :: (Int, Int)   -- ^ Current size of window. Grid size
                                        -- is calculated on each 'render' call,
                                        -- so grid resizes if window size changes.
    , stateZoom         :: Float        -- ^ Current zoom. See 'ZoomMode'.
    } deriving (Read, Eq, Ord, Show)

-- | The 'MouseMode' is enumeration for mouse modes.
data MouseMode
    = Idle          -- ^ Default mouse mode. It's active when all mouse buttons
                    -- are released.
    | AddMode       -- ^ Mouse mode for adding cells. Use left-click over a dead
                    -- cell to enable this mode. When this mode is enabled.
    | RemoveMode    -- ^ Mouse mode for removing cells. Use left-click over a live
                    -- cell to enable this mode.
    | ZoomMode      -- ^ Mouse mode for zooming. Use control-left-click drag or
                    -- right-click drag to change zoom. See 'stateZoom'.
    deriving (Read, Eq, Ord, Show)

-- | The 'Generation' type is a list of live cells in a generation.
type Generation = [Cell]

-- | The 'Cell' type represents live cell with coordinates in the grid. Note that
-- it uses coordinate system of the <http://hackage.haskell.org/package/gloss Gloss>
-- library.
type Cell = (Int, Int)

-- | The initial window size. See 'stateWindowSize'.
initialWindowSize :: (Int, Int)
initialWindowSize = (600, 600)

-- | The normal cell size. Actual cell size is calculated using this constant and
-- 'stateZoom'.
normalCellSize :: Float
normalCellSize = 40

-- | The window, where game is running.
window :: Display
window = InWindow "Game of Life" initialWindowSize (50, 50)

-- | The background color.
background :: Color
background = black

-- | The color of live cells.
liveCellColor :: Color
liveCellColor = white

-- | The color of dead cells.
deadCellColor :: Color
deadCellColor = background

-- | The color of cell borders.
borderColor :: Color
borderColor = white

-- | The count of frames per second. See 'update'.
fps :: Int
-- fps = 6
fps = 100

-- | The 'main' function is an entry point of this example.
main :: IO ()
main = play window background fps initialState render handleEvent update

{- | The 'initialState' function creates initial state with following configuration:

@
'State'
{ 'stateGeneration'   = []
, 'stateRunning'      = False
, 'stateMouseMode'    = 'Idle'
, 'statePrevMousePos' = (0, 0)
, 'stateWindowSize'   = 'initialWindowSize'
, 'stateZoom'         = 1
}
@
-}
initialState :: State
initialState
    = State
    { stateGeneration   = []
    , stateRunning      = False
    , stateMouseMode    = Idle
    , statePrevMousePos = (0, 0)
    , stateWindowSize   = initialWindowSize
    , stateZoom         = 1
    }

{- | The 'nextGeneration' function takes a 'Generation' and creates next generation
using rules described <#Rules here>.

For performance purposes this function updates all living cells in current then
generation and all their neighbors. This's better rather than updating each cell
in a big range, like 1000x1000.
-}
nextGeneration :: Generation -> Generation
nextGeneration cells = filter (willCellBeAlive cells) $ nub $ concatMap (\cell -> cell : neighbors cell) cells

-- | The 'willCellBeAlive' function checks if cell will be alive in the next
-- 'Generation' using rules described <#Rules here>.
willCellBeAlive :: Generation -> Cell -> Bool
willCellBeAlive cells cell =
  (cell `elem` cells && neighborsCount <= 3 && neighborsCount >= 2)
  || (cell `notElem` cells && neighborsCount == 3)
  where neighborsCount = length $ filter (`elem` cells) $ neighbors cell

-- | The 'neighbors' function returns coordinates of all neighbors of given cell,
-- which are the cells that are horizontally, vertically, or diagonally adjacent.
neighbors :: Cell -> [Cell]
neighbors (col, row) =
    [ (nCol, nRow) | nRow <- [row - 1..row + 1], nCol <- [col - 1..col + 1], not (nCol == col && nRow == row) ]

render :: State -> Picture
render State { stateGeneration = cells, stateWindowSize = (windowWidth, windowHeight), stateZoom = zoom }
    = pictures
    $ concatMap (\cell -> [renderCell cell (cell `elem` cells), renderNextCell cell (willCellBeAlive cells cell)])
    [ (col, row) | row <- [-rows `div` 2 .. rows `div` 2], col <- [-cols `div` 2 .. cols `div` 2] ]
    where
        actualCellSize = normalCellSize * zoom

        cols, rows :: Int
        cols = toEven $ floor $ fromIntegral windowWidth / actualCellSize
        rows = toEven $ floor $ fromIntegral windowHeight / actualCellSize

        toEven :: Integral a => a -> a
        toEven n
            | odd n     = n + 1
            | otherwise = n

        cellX :: Int -> Float
        cellX col = fromIntegral col * actualCellSize

        cellY :: Int -> Float
        cellY row = fromIntegral row * actualCellSize

        renderCell :: Cell -> Bool -> Picture
        renderCell (col, row) live
            = translate (cellX col) (cellY row) $ pictures
            [ color (if live then liveCellColor else deadCellColor) $ rectangleSolid actualCellSize actualCellSize
            , color borderColor $ rectangleWire actualCellSize actualCellSize
            ]

        renderNextCell :: Cell -> Bool -> Picture
        renderNextCell (col, row) live
            = translate (cellX col) (cellY row) $ scale 0.3 0.3
            $ color (if live then liveCellColor else deadCellColor) $ rectangleSolid actualCellSize actualCellSize

handleEvent :: Event -> State -> State

handleEvent (EventKey (MouseButton _) Up _ _) state = state { stateMouseMode = Idle }

handleEvent (EventKey (MouseButton LeftButton) Down Modifiers { ctrl = Down } mousePos) state =
    state { stateMouseMode = ZoomMode, statePrevMousePos = mousePos }
handleEvent (EventKey (MouseButton RightButton) Down _ mousePos) state =
    state { stateMouseMode = ZoomMode, statePrevMousePos = mousePos }
handleEvent (EventMotion mousePos) state@State { stateMouseMode = ZoomMode } = changeZoom mousePos state

handleEvent (EventKey (MouseButton LeftButton) Down _ (mouseX, mouseY))
            state@State { stateGeneration = cells, stateZoom = zoom } =
    if clickedCell `elem` cells
        then state { stateGeneration = delete clickedCell cells, stateMouseMode = RemoveMode }
        else state { stateGeneration = clickedCell:cells, stateMouseMode = AddMode }
    where
        clickedCell = (floor $ (mouseX + actualCellSize / 2) / actualCellSize, floor $ (mouseY + actualCellSize / 2) / actualCellSize)
        actualCellSize = normalCellSize * zoom
handleEvent (EventMotion (mouseX, mouseY)) state@(State cells _ editMode _ _ zoom) =
    case (clickedCell `elem` cells, editMode) of
        (True, RemoveMode) -> state { stateGeneration = delete clickedCell cells }
        (False, AddMode)   -> state { stateGeneration = clickedCell:cells }
        _                  -> state
    where
        actualCellSize = normalCellSize * zoom
        clickedCell = (floor $ (mouseX + actualCellSize / 2) / actualCellSize, floor $ (mouseY + actualCellSize / 2) / actualCellSize)
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) state@(State _ running Idle _ _ _) = state { stateRunning = not running }
handleEvent (EventKey (SpecialKey KeyUp)    Down _ _) state@State { stateGeneration = cells } = state { stateGeneration = map (\(x, y) -> (x, y - 1)) cells }
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) state@State { stateGeneration = cells } = state { stateGeneration = map (\(x, y) -> (x - 1, y)) cells }
handleEvent (EventKey (SpecialKey KeyDown)  Down _ _) state@State { stateGeneration = cells } = state { stateGeneration = map (\(x, y) -> (x, y + 1)) cells }
handleEvent (EventKey (SpecialKey KeyLeft)  Down _ _) state@State { stateGeneration = cells } = state { stateGeneration = map (\(x, y) -> (x + 1, y)) cells }
handleEvent (EventKey (Char 'c')  Down _ _) state = state { stateGeneration = [] }
handleEvent (EventResize windowSize) state = state { stateWindowSize = windowSize }
handleEvent _ cells = cells

changeZoom :: (Float, Float) -> State -> State
changeZoom mousePos@(_, mouseY) state@State { statePrevMousePos = (_, prevMouseY), stateZoom = zoom } =
    state { statePrevMousePos = mousePos, stateZoom = clamp 0.25 2.5 $ zoom + deltaMouseY / 1000 }
    where
        deltaMouseY = prevMouseY - mouseY
        clamp minN maxN n = max minN (min n maxN)

update :: Float -> State -> State
update _ state@State { stateGeneration = cells, stateRunning = True } = state { stateGeneration = nextGeneration cells }
update _ state = state
