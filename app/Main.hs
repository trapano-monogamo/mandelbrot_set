module Main (main) where

import Graphics.Gloss -- play

import FractalState
import Fractal

window :: Display
window = InWindow
  (fst $ coolFractals !! (fractalIndex fractalState))
  (sWidth fractalState, sHeight fractalState) (1000,200)

main :: IO ()
main = do
  play window
       black
       60
       fractalState
       renderFractalState
       updateFractalState
       playFractalState
