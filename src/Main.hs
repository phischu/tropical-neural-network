module Main where

import Test.QuickCheck (
  Gen, oneof, choose)
import Test.QuickCheck.Random (
  mkQCGen)
import Test.QuickCheck.Gen (
  Gen(MkGen))

import Diagrams.Prelude (
  Diagram, circle, position, rect, rasterDia,
  atop, scaleUToX,
  fillColor, lineColor, orange, blue, white, transparent, opaque, blend,
  dims)
import Diagrams.Backend.Rasterific (
  renderRasterific, Rasterific)

import Linear (
  V2(V2))
import Linear.Affine (
  Point(P))
import Linear.Matrix (
  M22, identity, (!*))

import Control.Monad (
  replicateM)
import Data.Function (
  (&))


data TrainingSample = TrainingSample {
  sampleInput :: V2 Double,
  sampleOutput :: Bool}
    deriving (Show)

trainingData :: [TrainingSample]
trainingData =
  runGen generateTrainingSamples

generateTrainingSamples :: Gen [TrainingSample]
generateTrainingSamples =
  replicateM 50 generateTrainingSample

generateTrainingSample :: Gen TrainingSample
generateTrainingSample = oneof [
  generateTrainingSample1,
  generateTrainingSample2]

generateTrainingSample1 :: Gen TrainingSample
generateTrainingSample1 = do
  x1 <- choose (-1,0)
  x2 <- choose (-1,0)
  return (TrainingSample (V2 x1 x2) False)

generateTrainingSample2 :: Gen TrainingSample
generateTrainingSample2 = do
  x1 <- choose (0,1)
  x2 <- choose (0,1)
  return (TrainingSample (V2 x1 x2) True)

runGen :: Gen a -> a
runGen (MkGen gen) =
  gen (mkQCGen 489) 30


renderTrainingSamples :: [TrainingSample] -> Diagram Rasterific
renderTrainingSamples trainingSamples = position (do
  TrainingSample x y <- trainingSamples
  let yColor = case y of
        False -> blue
        True -> orange
  let sampleDiagram = circle 0.02 & fillColor yColor
  return (P x, sampleDiagram))



data Net a = Net (M22 a)

runNet :: (Fractional a) => Net a -> V2 a -> a
runNet (Net m1) x = sum (m1 !* x)


exampleNet :: (Num a) => Net a
exampleNet = Net identity


renderNet :: Net Double -> Diagram Rasterific
renderNet net = let

  resolution = 600

  outputColor r =
    opaque (blend (logistic r) orange blue)

  logistic x = recip (1 + exp (negate x))

  pointRaster i j = let
    x1 = 2 * (fromIntegral i / fromIntegral resolution) - 1
    x2 = negate (2 * (fromIntegral j / fromIntegral resolution) - 1)
    in V2 x1 x2

  pointColor i j =
    outputColor (runNet net (pointRaster i j))

  in rasterDia pointColor resolution resolution & scaleUToX 2


main :: IO ()
main = do
  let whiteBackground =
        rect 2.1 2.1 & fillColor white & lineColor transparent
  let outputDiagram =
        renderTrainingSamples trainingData `atop`
        renderNet exampleNet `atop`
        whiteBackground
  let sizeSpec =
        dims (V2 600 600)
  renderRasterific "training_samples.png" sizeSpec outputDiagram


