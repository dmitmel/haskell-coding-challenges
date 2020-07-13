{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

{-|
Module      : Graphics.Gloss.Raster.Pixels
Copyright   : (c) 2017 Dmytro Meleshko
License     : Apache-2.0
Maintainer  : dmytro.meleshko@gmail.com
Stability   : stable

Fast 'Picture' rendering from list of pixels.
-}

module Graphics.Gloss.Raster.Pixels where

import Data.ByteString (pack)
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap

-- | The 'makePicture' function takes 'RowOrder', size of 'Picture' and list of
-- pixels (rows go first), and returns generated image with these pixels. If
-- there're more or less pixels than expected, image will be generated only frome
-- those pixels.
makePicture :: RowOrder -> (Int, Int) -> [Color] -> Picture
makePicture !order (!width, !height) !pixels = bitmapOfByteString
    width height
    (BitmapFormat order PxRGBA)
    (pack $ concatMap (\pixel ->
        let (r, g, b, a) = rgbaOfColor pixel
            !r' = truncate $ r * 255
            !g' = truncate $ g * 255
            !b' = truncate $ b * 255
            !a' = truncate $ a * 255
        in [r', g', b', a']) pixels)
    False    -- don't cache this in texture memory
{-# INLINE makePicture #-}
