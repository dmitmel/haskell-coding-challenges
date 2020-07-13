module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Math

type Generation = [Cell]

type Cell = Bool

type Ruleset = (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool)

windowSize :: (Int, Int)
windowWidth, windowHeight :: Int
windowSize@(windowWidth, windowHeight) = (600, 800)

windowFloatWidth, windowFloatHeight :: Float
windowFloatWidth = fromIntegral windowWidth
windowFloatHeight = fromIntegral windowHeight

generationSize, generationsCount :: Int
generationSize = round $ windowFloatWidth / cellSize
generationsCount = round $ windowFloatHeight / cellSize

cellSize :: Float
cellSize = 5

ruleset :: Ruleset
-- ruleset = (False, True,  True,  True,  True, False, True,  True )   -- Rule 222
-- ruleset = (False, True,  True,  True,  True,  True, False, True )   -- Rule 190
-- ruleset = (False, True,  True,  True,  True, False, False, False)   -- Rule 30
-- ruleset = (False, True,  True,  True,  True,  True, True,  False)   -- Rule 110
ruleset = (False, True,  False, True,  True, False, True,  False)   -- Rule 90

window :: Display
window = InWindow "Elementary Cellular Automata" windowSize (50, 50)

background :: Color
background = black

cellColor :: Color
cellColor = white

fps :: Int
fps = 24

main :: IO ()
main = simulate window background fps [] render update

initialGeneration :: Generation
initialGeneration =
    replicate (generationSize `div` 2) False ++
    [True] ++
    replicate (generationSize - generationSize `div` 2) False

nextGeneration :: Generation -> Generation
nextGeneration cells =
    zipWith (\cell index ->
        let left  = cellAt (index - 1)
            right = cellAt (index + 1)
        in rule left cell right
    ) cells [0..]
  where
    cellAt :: Int -> Cell
    cellAt n
      | n >= 0 && n < length cells = cells !! repeatN (0, length cells) n
      | otherwise                  = False

    rule :: Bool -> Bool -> Bool -> Bool
    rule False False False = rule1
    rule False False True  = rule2
    rule False True  False = rule3
    rule False True  True  = rule4
    rule True  False False = rule5
    rule True  False True  = rule6
    rule True  True  False = rule7
    rule True  True  True  = rule8

    (rule1, rule2, rule3, rule4, rule5, rule6, rule7, rule8) = ruleset

nextGenerations :: Generation -> Int -> [Generation]
nextGenerations firstGen count
    | count > 0 =
        let nextGen = nextGeneration firstGen
        in nextGen : nextGenerations nextGen (count - 1)
    | otherwise = []

render :: [Generation] -> Picture
render generations =
    translate (-windowFloatWidth / 2) (-windowFloatHeight / 2) $
    pictures $
    concat $
    zipWith (\gen genIndex ->
        map (translate 0 (genIndex * cellSize)) $ renderGeneration gen
    ) generations [0..]
  where
    renderGeneration :: Generation -> [Picture]
    renderGeneration gen =
        concat $
        zipWith (\cell cellIndex ->
            [translate (cellIndex * cellSize) 0 $ renderCell cell | cell]
        ) gen [0..]

    renderCell :: Cell -> Picture
    renderCell live
        | live      = color cellColor $ rectangleSolid cellSize cellSize
        | otherwise = blank

update :: ViewPort -> Float -> [Generation] -> [Generation]
update _ _ [] = [initialGeneration]
update _ _ (firstGen:generations) =
    take generationsCount $ nextGeneration firstGen : firstGen : generations
