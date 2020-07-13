{-
Purple Rain (https://youtu.be/KkyIDI6rQJI?list=PLRqwX-V7Uu6ZiZxtDDRCi6uhfTH4FilpH)
-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import System.Random

data Droplet
    = Droplet
    { dropletX              :: Float
    , dropletY              :: Float
    , dropletLength         :: Float
    , dropletFallSpeedSpeed :: Float
    } deriving (Read, Eq, Ord, Show)

data Rain
    = Rain
    { rainDroplets  :: [Droplet]
    -- Random generator is for creating new droplets on random positions
    , rainRandomGen :: StdGen
    } deriving (Read, Show)

dropletsCount :: Int
dropletsCount = 600

dropletLengthRange :: (Float, Float)
dropletLengthRange = (10, 20)

dropletFallSpeedRange :: (Float, Float)
dropletFallSpeedRange = (30, 120)

-- Droplet fall speed increase per second
dropletFallSpeedIncreaseSpeed :: Float
dropletFallSpeedIncreaseSpeed = 100

windowSize :: (Int, Int)
windowWidth, windowHeight :: Int
windowSize@(windowWidth, windowHeight) = (640, 360)

windowFloatWidth, windowFloatHeight :: Float
windowFloatWidth = fromIntegral windowWidth
windowFloatHeight = fromIntegral windowHeight

minDropletY :: Float
minDropletY = -windowFloatHeight / 2

window :: Display
window = InWindow "Purple Rain" windowSize (50, 50)

background :: Color
background = makeColorI 230 230 250 255

dropletColor :: Color
dropletColor = makeColorI 138 43 226 255

fps :: Int
fps = 30

main :: IO ()
main = simulate window background fps createRain render update

createRain :: Rain
createRain = let (droplets, rndGen) = createDroplets dropletsCount $ mkStdGen 0 in Rain droplets rndGen
    where
        createDroplets :: Int -> StdGen -> ([Droplet], StdGen)
        createDroplets count rndGen
            | count > 0 =
                let (droplet, rndGen2) = createDroplet rndGen
                    (nextDroplets, lastGen) = createDroplets (count - 1) rndGen2
                in (droplet:nextDroplets, lastGen)
            | otherwise = ([], rndGen)

createDroplet :: StdGen -> (Droplet, StdGen)
createDroplet rndGen =
    let (x, rndGen2)      = randomR (-windowFloatWidth / 2, windowFloatWidth / 2) rndGen
        (y, rndGen3)      = randomR (windowFloatHeight / 2 + 50, windowFloatHeight / 2 + 500) rndGen2
        (len, rndGen4)    = randomR dropletLengthRange rndGen3
        (fallSpeed, rndGen5) = randomR dropletFallSpeedRange rndGen4
    in (Droplet x y len fallSpeed, rndGen5)

render :: Rain -> Picture
render (Rain droplets _) = pictures $ map (\(Droplet x y len _) -> color dropletColor $ line [(x, y), (x, y - len)]) droplets

update :: ViewPort -> Float -> Rain -> Rain
-- Delta time is the time in seconds between previous and current frames
update _ deltaTime (Rain droplets rndGen) = foldl updateDroplet (Rain [] rndGen) droplets
    where
        updateDroplet :: Rain -> Droplet -> Rain
        updateDroplet (Rain droplets rndGen) (Droplet x y len fallSpeed)
            | newY < minDropletY = let (droplet, rndGen2) = createDroplet rndGen in Rain (droplet:droplets) rndGen2
            | otherwise = Rain (Droplet x newY len newFallSpeed : droplets) rndGen
            {-
            Multiplying value by delta time gives speed in value per second.
            For example, following code will move pseudo-object by 10 units per second:

                object.position = object.position + 10 * deltaTime

            And this speed is same for any FPS (probably)
            -}
            where
                newY      = y - fallSpeed * deltaTime
                newFallSpeed = fallSpeed + dropletFallSpeedIncreaseSpeed * deltaTime
