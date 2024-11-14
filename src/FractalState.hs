module FractalState
( Fractal
, FractalState(..)
, fractalState
, setThreshold
, setMaxIterations
, changeFractal
, coolFractals
, getFractal
, setScaleDimensions
, setScreenDimensions
, moveFractal
, setFractalPosition
, setPictureDimensions
) where


import Data.Complex


type Fractal = (Complex Float -> Complex Float -> Complex Float)


coolFractals :: [(String, Fractal)]
coolFractals = [ ("mandelbrot",         (\c z -> z**2 + c)),                 -- 0
                 ("pine processionary", (\c z -> (exp z) ** 2 + c)),         -- 1
                 ("curious creatures",  (\c z -> (1 / (exp z) ** 2) + c)),   -- 2
                 ("idk",                (\c z -> (z / (exp z)) + c)),        -- 3
                 ("starry night",       (\c z -> (exp z) ** (0 :+ 2) + c)),  -- 4
                 ("idk 2",              (\c z -> (exp z) / z + c)),          -- 5
                 ("ant mandibles",      (\c z -> z / (exp $ 1 / z) + c)),    -- 6
                 ("legged star",        (\c z -> (log z) / (exp z) + c)),    -- 7
                 ("ghostly dust",       (\c z -> (z * log z) ** c)),         -- 8
                 ("idk 3",              (\c z -> (z * sin z) + c))           -- 9
               ]

getFractal :: Int -> Fractal
getFractal idx = snd $ coolFractals !! idx


data FractalState =
  FractalState { sWidth :: Int, sHeight :: Int
               , pWidth :: Int, pHeight :: Int
               , scaleX :: Float, scaleY :: Float
               , posX :: Float, posY :: Float
               , threshold :: Float
               , maxIterations :: Int
               , fractalIndex :: Int
               , fractal :: Fractal
               }

fractalState :: FractalState
fractalState =
  FractalState { sWidth = 1400, sHeight = 1400
               , pWidth = 700, pHeight = 700
               , scaleX = 30.5, scaleY = 30.5
               , posX = 0, posY = 0
               , threshold = 10.0
               , maxIterations = 10
               , fractalIndex = 0
               , fractal = snd $ coolFractals !! 0
               }

moveFractal :: Float -> Float -> FractalState -> FractalState
moveFractal x y state =
  FractalState { sWidth = sWidth state, sHeight = sHeight state
               , pWidth = pWidth state, pHeight = pHeight state
               , scaleX = scaleX state, scaleY = scaleY state
               , posX = dx + posX state, posY = dy + posY state
               , threshold = threshold state
               , maxIterations = maxIterations state
               , fractalIndex = fractalIndex state
               , fractal = fractal state
               }
  where dx =  x / scaleX state
        dy = -y / scaleY state

setFractalPosition :: Float -> Float -> FractalState -> FractalState
setFractalPosition x y state =
  FractalState { sWidth = sWidth state, sHeight = sHeight state
               , pWidth = pWidth state, pHeight = pHeight state
               , scaleX = scaleX state, scaleY = scaleY state
               , posX = x, posY = y
               , threshold = threshold state
               , maxIterations = maxIterations state
               , fractalIndex = fractalIndex state
               , fractal = fractal state
               }

setScreenDimensions :: Int -> Int -> FractalState -> FractalState
setScreenDimensions w h state =
  FractalState { sWidth = w, sHeight = h
               , pWidth = pWidth state, pHeight = pHeight state
               , scaleX = scaleX state, scaleY = scaleY state
               , posX = posX state, posY = posY state
               , threshold = threshold state
               , maxIterations = maxIterations state
               , fractalIndex = fractalIndex state
               , fractal = fractal state
               }

setPictureDimensions :: Int -> Int -> FractalState -> FractalState
setPictureDimensions w h state =
  FractalState { sWidth = sWidth state, sHeight = sHeight state
               , pWidth = w, pHeight = h
               , scaleX = scaleX state, scaleY = scaleY state
               , posX = posX state, posY = posY state
               , threshold = threshold state
               , maxIterations = maxIterations state
               , fractalIndex = fractalIndex state
               , fractal = fractal state
               }

setScaleDimensions :: Float -> Float -> FractalState -> FractalState
setScaleDimensions sx sy state =
  FractalState { sWidth = sWidth state, sHeight = sHeight state
               , pWidth = pWidth state, pHeight = pHeight state
               , scaleX = sx, scaleY = sy
               , posX = posX state, posY = posY state
               , threshold = threshold state
               , maxIterations = maxIterations state
               , fractalIndex = fractalIndex state
               , fractal = fractal state
               }

setThreshold :: Float -> FractalState -> FractalState
setThreshold tr state =
  FractalState { sWidth = sWidth state, sHeight = sHeight state
               , pWidth = pWidth state, pHeight = pHeight state
               , scaleX = scaleX state, scaleY = scaleY state
               , posX = posX state, posY = posY state
               , threshold = tr
               , maxIterations = maxIterations state
               , fractalIndex = fractalIndex state
               , fractal = fractal state
               }

setMaxIterations :: Int -> FractalState -> FractalState
setMaxIterations iters state =
  FractalState { sWidth = sWidth state, sHeight = sHeight state
               , pWidth = pWidth state, pHeight = pHeight state
               , scaleX = scaleX state, scaleY = scaleY state
               , posX = posX state, posY = posY state
               , threshold = threshold state
               , maxIterations = iters
               , fractalIndex = fractalIndex state
               , fractal = fractal state
               }

changeFractal :: Int -> FractalState -> FractalState
changeFractal idx state = 
  FractalState { sWidth = sWidth fractalState, sHeight = sHeight fractalState
               , pWidth = pWidth fractalState, pHeight = pHeight fractalState
               , scaleX = scaleX fractalState, scaleY = scaleY fractalState
               , posX = posX fractalState, posY = posY fractalState
               , threshold = threshold fractalState
               , maxIterations = maxIterations fractalState
               , fractalIndex = idx
               , fractal = snd $ coolFractals !! idx
               }
