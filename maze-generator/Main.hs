{-
Maze Generator (https://youtu.be/_p5IH0L63wo?list=PLRqwX-V7Uu6ZiZxtDDRCi6uhfTH4FilpH)

This generator uses the Depth-First Search algorithm, which is described on Wikipedia:
https://en.wikipedia.org/wiki/Maze_generation_algorithm#Recursive_backtracker
-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import System.Random

type CellCoord = (Int, Int)

data Cell
    = Cell
    { cellCoord      :: CellCoord
    , cellVisited    :: Bool
    , cellTopWall    :: Bool
    , cellRightWall  :: Bool
    , cellBottomWall :: Bool
    , cellLeftWall   :: Bool
    } deriving (Read, Eq, Ord, Show)

data Maze
    = Maze
    { mazeCells       :: [Cell]
    , mazeCurrentCell :: CellCoord
    , mazeHistory     :: [CellCoord]  -- When there're no empty cells, generator uses history to step back
    , mazeRandomGen   :: StdGen       -- Random generator is for choosing movement direction
    } deriving (Read, Show)

cellSize :: Float
cellSize = 40

windowSize :: (Int, Int)
windowWidth, windowHeight :: Int
windowSize@(windowWidth, windowHeight) = (600, 600)

cols, rows :: Int
cols = floor $ fromIntegral windowWidth / cellSize
rows = floor $ fromIntegral windowHeight / cellSize

gridWidth, gridHeight :: Float
gridWidth = fromIntegral cols * cellSize
gridHeight = fromIntegral rows * cellSize

window :: Display
window = InWindow "Maze Generator" windowSize (50, 50)

background :: Color
background = makeColorI 51 51 51 255

visitedCellColor :: Color
visitedCellColor = makeColorI 255 0 255 100

wallColor :: Color
wallColor = white

fps :: Int
fps = 3

main :: IO ()
main = do
    stdGen <- getStdGen
    let maze = createMaze { mazeRandomGen = stdGen }
    simulate window background fps maze render update

createMaze :: Maze
createMaze
    = Maze
    { mazeCells       = [ createCell { cellCoord = (col, row) } | row <- [0..rows - 1], col <- [0..cols - 1] ]
    , mazeCurrentCell = (0, 0)
    , mazeHistory     = []
    , mazeRandomGen   = undefined
    }
    where
        createCell
            = Cell
            { cellCoord      = (0, 0)
            , cellVisited    = False
            , cellTopWall    = True
            , cellRightWall  = True
            , cellBottomWall = True
            , cellLeftWall   = True
            }

-- Get cell at specified position
(#) :: Maze -> CellCoord -> Cell
(Maze cells _ _ _) # (col, row) = cells !! (col + row * cols)

-- Update cell on position from new value.
(#=) :: Maze -> Cell -> Maze
maze@(Maze cells _ _ _) #= cell@(Cell (col, row) _ _ _ _ _) =
    maze { mazeCells = setAt (col + row * cols) cell cells }
    where
        setAt :: Int -> a -> [a] -> [a]
        setAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs

render :: Maze -> Picture
render (Maze cells (currentCol, currentRow) _ _)
    = translate (-gridWidth / 2) (gridHeight / 2) $ pictures
    $ translate (cellX currentCol) (cellY currentRow)
        (color (makeColorI 0 0 255 100) (rectangleSolid cellSize cellSize))
    : map renderCell cells
    where
        cellX :: Int -> Float
        cellX col = fromIntegral col * cellSize + cellSize / 2

        cellY :: Int -> Float
        cellY row = -fromIntegral row * cellSize - cellSize / 2

        renderCell :: Cell -> Picture
        renderCell cell@(Cell (col, row) visited _ _ _ _)
            | visited
                = translate (cellX col) (cellY row) $ pictures
                $ color visitedCellColor (rectangleSolid cellSize cellSize)
                : renderWalls cell
            | otherwise
                = translate (cellX col) (cellY row) $ pictures
                $ color background (rectangleSolid cellSize cellSize)
                : renderWalls cell

        renderWalls :: Cell -> [Picture]
        renderWalls (Cell _ _ topWall rightWall bottomWall leftWall)
            = map (translate (-cellSize / 2) (cellSize / 2) . color wallColor . line) $
            [ [(0,                0), (cellSize,         0)] | topWall    ] ++
            [ [(cellSize,         0), (cellSize, -cellSize)] | rightWall  ] ++
            [ [(cellSize, -cellSize), (0,        -cellSize)] | bottomWall ] ++
            [ [(0,        -cellSize), (0,                0)] | leftWall   ]

update :: ViewPort -> Float -> Maze -> Maze
update _ _ maze@(Maze _ currentCellCoord@(currentCol, currentRow) history rndGen)
    | null neighbors = case history of
        []           -> maze
        (step:steps) -> maze { mazeCurrentCell = step, mazeHistory = steps }
    | otherwise =
        let (neighborIndex, rndGen2) = randomR (0, length neighbors - 1) rndGen
            nextCell                 = neighbors !! neighborIndex
            (updatedCurrentCell, updatedNextCell) = removeWalls nextCell
        in maze { mazeCurrentCell = cellCoord nextCell, mazeHistory = currentCellCoord:history, mazeRandomGen = rndGen2 }
            #= updatedCurrentCell { cellVisited = True } #= updatedNextCell { cellVisited = True }
    where
        neighbors :: [Cell]
        neighbors = filter (not . cellVisited) $ map (maze #) $
            [ (currentCol - 1,     currentRow) | currentCol > 0        ] ++
            [ (currentCol + 1,     currentRow) | currentCol < cols - 1 ] ++
            [ (currentCol,     currentRow - 1) | currentRow > 0        ] ++
            [ (currentCol,     currentRow + 1) | currentRow < rows - 1 ]

        removeWalls :: Cell -> (Cell, Cell)
        removeWalls nextCell
            | currentRow > nextRow = (currentCell { cellTopWall    = False }, nextCell { cellBottomWall = False })
            | currentCol < nextCol = (currentCell { cellRightWall  = False }, nextCell { cellLeftWall   = False })
            | currentRow < nextRow = (currentCell { cellBottomWall = False }, nextCell { cellTopWall    = False })
            | currentCol > nextCol = (currentCell { cellLeftWall   = False }, nextCell { cellRightWall  = False })
            | otherwise            = (currentCell, nextCell)
            where
                currentCell        = maze # currentCellCoord
                (nextCol, nextRow) = cellCoord nextCell
