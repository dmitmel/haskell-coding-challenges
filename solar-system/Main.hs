{-
Solar System (https://youtu.be/l8SiJ-RmeHU?list=PLRqwX-V7Uu6ZiZxtDDRCi6uhfTH4FilpH)
-}

module Main where

import Data.Fixed                   (mod')
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import System.Random

data Planet
    = Planet
    { planetRadius       :: Float
    , planetDistance     :: Float
    , planetOrbitalSpeed :: Float    -- Orbital speed in degrees per second
    , planetAngle        :: Float    -- Angle in degrees
    , planetMoons        :: [Planet]
    } deriving (Read, Eq, Ord, Show)

radiusOfAnyPlanet :: Float
radiusOfAnyPlanet = 50

sunMoonsCount :: Int
sunMoonsCount = 5

moonsCountRange :: (Int, Int)
moonsCountRange = (0, 4)

distanceRange :: (Float, Float)
distanceRange = (0, 500)

orbitalSpeedRange :: (Float, Float)
orbitalSpeedRange = (60, 140)

moonSizeMultiplier :: Float
moonSizeMultiplier = 0.75

windowSize :: (Int, Int)
windowWidth, windowHeight :: Int
windowSize@(windowWidth, windowHeight) = (600, 600)

windowFloatWidth, windowFloatHeight :: Float
windowFloatWidth = fromIntegral windowWidth
windowFloatHeight = fromIntegral windowHeight

window :: Display
window = InWindow "Solar System" windowSize (50, 50)

background :: Color
background = black

planetColor :: Color
planetColor = makeColorI 255 255 255 100

fps :: Int
fps = 30

main :: IO ()
main = do
    rndGen <- getStdGen
    let (sun, _) = createPlanet 1 rndGen
    simulate window background fps sun render update

createPlanet :: Int -> StdGen -> (Planet, StdGen)
createPlanet level rndGen
    | level == 1 =
        let (moons, rndGen2) = createMoons sunMoonsCount rndGen
        in (Planet radiusOfAnyPlanet 0 0 0 moons, rndGen2)
    | level < 4 =
        let (planet, rndGen2)     = createPlanetOnRandomPosition rndGen
            (moonsCount, rndGen3) = randomR moonsCountRange rndGen2
            (moons, rndGen4)      = createMoons moonsCount rndGen3
        in (planet { planetMoons = moons }, rndGen4)
    | otherwise = createPlanetOnRandomPosition rndGen
    where
        createMoons :: Int -> StdGen -> ([Planet], StdGen)
        createMoons moonsCount rndGen
            | moonsCount > 0 =
                let (moon, rndGen2)  = createPlanet (level + 1) rndGen
                    (moons, lastGen) = createMoons (moonsCount - 1) rndGen2
                in (moon:moons, lastGen)
            | otherwise = ([], rndGen)

        createPlanetOnRandomPosition :: StdGen -> (Planet, StdGen)
        createPlanetOnRandomPosition rndGen =
            let (distance, rndGen2)     = randomR distanceRange rndGen
                (orbitalSpeed, rndGen3) = randomR orbitalSpeedRange rndGen2
                (angle, rndGen4)        = randomR (0, 360) rndGen3
            in (Planet radiusOfAnyPlanet distance orbitalSpeed angle [], rndGen4)

render :: Planet -> Picture
render (Planet radius _ _ _ moons)
    = pictures $ color planetColor (circleSolid radius) : map renderMoon moons
    where
        renderMoon :: Planet -> Picture
        renderMoon moon@(Planet _ moonDistance _ moonAngle _)
            = scale moonSizeMultiplier moonSizeMultiplier $ pictures
            [ color planetColor $ circle (moonDistance + radius)                    -- Orbit of moon
            , rotate moonAngle $ translate 0 (moonDistance + radius) $ render moon  -- Moon
            ]

update :: ViewPort -> Float -> Planet -> Planet
update viewPort deltaTime planet@(Planet _ _ orbitalSpeed angle moons)
    = planet
    -- Angle mustn't be greater than 360, so newAngle % 360 solves this problem
    { planetAngle = newAngle `mod'` 360   -- Function mod' works like mod, but it's for fractional numbers
    , planetMoons = map (update viewPort deltaTime) moons
    }
    where newAngle = angle + deltaTime * orbitalSpeed
