module CodingChallanges.LSystems.SVG where

import Control.Monad (forM_)
import System.IO
import Math.LinearAlgebra.Vector.Vector2
import CodingChallanges.LSystems.Config
import CodingChallanges.LSystems.Turtle (TurtlePath)

pathsToSVG :: Config -> [TurtlePath] -> IO ()
pathsToSVG config paths =
    case configOutputSVG config of
        ""   -> hPathsToSVG stdout config paths
        file -> withFile file WriteMode (\handle -> hPathsToSVG handle config paths)

hPathsToSVG :: Handle -> Config -> [TurtlePath] -> IO ()
hPathsToSVG handle config paths = do
    hPutStr handle "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
    hPutStr handle "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\""
    hPutStr handle " \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"

    hPutStr handle "<svg version=\"1.1\" baseProfile=\"full\" xmlns=\"http://www.w3.org/2000/svg\""
    hPutStr handle " xmlns:xlink=\"http://www.w3.org/1999/xlink\" xmlns:ev=\"http://www.w3.org/2001/xml-events\""

    let (Size (width, height)) = configSVGSize config
    hPutStr handle $ " width=\"" ++ show width ++ "\" height=\"" ++ show height ++ "\">"
    hPutStr handle $ "<g transform=\"translate(" ++ show (width / 2) ++ "," ++ show (height / 2) ++ ")"
    hPutStr handle " scale(1,-1)\">"

    forM_ paths (\(V2 firstX firstY:points) -> do
        hPutStr handle $ "<path fill=\"none\" stroke=\"black\" d=\"M" ++ show firstX ++ " " ++ show firstY
        forM_ points (\(V2 x y) -> hPutStr handle $ " L" ++ show x ++ " " ++ show y)
        hPutStr handle "\"/>")

    hPutStrLn handle "</g></svg>"
