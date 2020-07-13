module CodingChallanges.LSystems.Config where

import System.Console.ArgParser
import System.Console.ArgParser.Params

data Config
    = DrawModeConfig { configFile          :: FilePath
                     , configIterations    :: Int }
    | TurtleModeConfig { configFile          :: FilePath
                       , configIterations    :: Int
                       , configFPS           :: Int
                       , configStepsPerFrame :: Int }
    | SVGModeConfig { configFile       :: FilePath
                    , configIterations :: Int
                    , configOutputSVG  :: FilePath
                    , configSVGSize    :: Size }
    deriving (Read, Eq, Ord, Show)

newtype Size = Size
    { getSize :: (Float, Float)
    } deriving (Eq, Ord)

instance Read Size where
    readsPrec _ s1 =
        [ (Size (width, height), s4)
        | (width,  s2) <- reads s1
        , ('x':    s3) <- [s2]
        , (height, s4) <- reads s3
        ]

instance Show Size where
    show (Size (width, height)) = show width ++ "x" ++ show height

configParser :: IO (CmdLnInterface Config)
configParser = mkSubParser
    [ ("draw",   mkDefaultApp drawModeParser   "draw"  )
    , ("turtle", mkDefaultApp turtleModeParser "turtle")
    , ("svg",    mkDefaultApp svgModeParser    "svg"   )
    ]

drawModeParser :: ParserSpec Config
drawModeParser = DrawModeConfig
    `parsedBy` reqPos "file" `Descr` "JSON definition file of a L-system"
    `andBy` optFlag 4 "iterations"

turtleModeParser :: ParserSpec Config
turtleModeParser = TurtleModeConfig
    `parsedBy` reqPos "file" `Descr` "JSON definition file of a L-system"
    `andBy` optFlag 4 "iterations"
    `andBy` optFlag 50 "fps"
    `andBy` optFlag 10 "steps-per-frame"

svgModeParser :: ParserSpec Config
svgModeParser = SVGModeConfig
    `parsedBy` reqPos "file" `Descr` "JSON definition file of a L-system"
    `andBy` optFlag 4 "iterations"
    `andBy` optFlag "" "output-svg" `Descr` "output SVG file"
    `andBy` readReqPos "svg-size"

readReqPos :: Read a => Key -> StdArgParam a
readReqPos key = StdArgParam Mandatory Pos key (SingleArgParser $ \arg -> case reads arg of
    [(x, "")] -> Right x
    _         -> Left $ "Could not parse parameter " ++ key ++ ". Unable to convert " ++ arg)
