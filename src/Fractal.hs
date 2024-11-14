module Fractal
( computeFractal
, pixelToComplex
, colorToBytes
, rgba
, colorCode
, fractalImage
, updateFractalState
, renderFractalState
, playFractalState
) where


import FractalState

import Graphics.Gloss
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game     -- Event, IO version of play function

import Data.Complex
import Data.Word (Word8, Word32)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Bits (shiftR, shiftL, (.&.), (.|.))


computeFractal :: FractalState -> Complex Float -> Int
computeFractal state start = go start start 0
  where go :: Complex Float -> Complex Float -> Int -> Int
        go z c iters
          | iters >= (maxIterations state) = (maxIterations state)
          | (magnitude z) >= (threshold state) = iters
          | otherwise = go (fractal c z) c (iters + 1)
          where fractal = getFractal $ fractalIndex state

pixelToComplex :: FractalState -> (Int,Int) -> Complex Float
pixelToComplex state (py,px) = cx :+ cy
  where x = fromIntegral px
        y = fromIntegral py
        dx = (fromIntegral $ pWidth state) / 2
        dy = (fromIntegral $ pHeight state) / 2
        cx = (posX state) + (1 / scaleX state) * (x - dx)
        cy = (posY state) + (1 / scaleY state) * (y - dy)

colorToBytes :: Word32 -> [Word8]
colorToBytes w = [ fromIntegral $ (w `shiftR` 24),
                   fromIntegral $ (w `shiftR` 16) .&. 0xFF,
                   fromIntegral $ (w `shiftR`  8) .&. 0xFF,
                   fromIntegral $  w              .&. 0xFF ]

rgba :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
rgba r g b a = (fromIntegral r `shiftL` 24) .|.
               (fromIntegral g `shiftL` 16) .|.
               (fromIntegral b `shiftL`  8) .|.
               fromIntegral a

colorCode :: FractalState -> Int -> Word32
colorCode state iters =
  let t = (fromIntegral iters / (fromIntegral $ maxIterations state)) :: Float
      r = round (255 * t) :: Word8
      g = 0 :: Word8
      b = round (255 * (1 - t)) :: Word8
  in rgba r g b 0xFF

fractalImage :: FractalState -> ByteString
fractalImage state = (B.pack . concat) pixels
  where pixels :: [[Word8]]
        pixels = map (colorToBytes . (colorCode state) . (computeFractal state) . (pixelToComplex state)) pts
        w = pWidth state
        h = pHeight state
        pts = [(y,x) | y <- [1..h], x <- [1..w]]


-- GLOSS functions

updateFractalState :: Event -> FractalState -> FractalState
updateFractalState = go
  where go (EventKey k Down _ _) = handleKeyPress k
        go _ = id
        handleKeyPress k state = case k of
          Char 'm' -> setMaxIterations ((maxIterations state) + 10) state
          Char 'M' -> setMaxIterations ((maxIterations state) - 10) state
          Char 't' -> setThreshold ((threshold state) + 10) state
          Char 'T' -> setThreshold ((threshold state) - 10) state
          Char 'i' -> changeFractal ((fractalIndex state) + 1) state
          Char 'I' -> changeFractal ((fractalIndex state) - 1) state
          Char 'w' -> moveFractal     0    20 state
          Char 's' -> moveFractal     0 (-20) state
          Char 'a' -> moveFractal (-20)     0 state
          Char 'd' -> moveFractal    20     0 state
          Char 'p' -> setPictureDimensions ((pWidth state) + 100) ((pHeight state) + 100) state
          Char 'P' -> setPictureDimensions ((pWidth state) - 100) ((pHeight state) - 100) state
          SpecialKey KeyUp -> setScaleDimensions ((scaleX state) * 2) ((scaleY state) * 2) state
          SpecialKey KeyDown -> setScaleDimensions ((scaleX state) / 2) ((scaleY state) / 2) state
          _ -> state

renderFractalState :: FractalState -> Picture
renderFractalState state = pictures [scale (1 / tx) (1 / ty) image, infoPic]
  where infoPic = color white $ translate (-sw/2.1) (sh/2.1) $ scale 0.2 0.2 $ text
          $ "scale: " ++ (show ((scaleX state),(scaleY state)))
          ++ "   threshold: " ++ (show $ threshold state)
          ++ "   max iterations: " ++ (show $ maxIterations state)
          ++ "   picture size: " ++ (show ((pWidth state),(pHeight state)))
        image = bitmapOfByteString
          (pWidth state) (pHeight state)
          (BitmapFormat TopToBottom PxRGBA) (fractalImage state) True
        sw = fromIntegral $ sWidth state
        sh = fromIntegral $ sHeight state
        iw = fromIntegral $ pWidth state
        ih = fromIntegral $ pHeight state
        tx = iw / sw
        ty = ih / sh

playFractalState :: Float -> FractalState -> FractalState
playFractalState _ = id