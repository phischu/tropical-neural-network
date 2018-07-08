module Main where

import Test.QuickCheck (
  Gen, oneof, choose)
import Test.QuickCheck.Random (
  mkQCGen)
import Test.QuickCheck.Gen (
  Gen(MkGen))

import Diagrams.Prelude (
  Diagram, circle, position, rect,
  atop,
  fillColor, lineColor, orange, blue, white, transparent,
  dims)
import Diagrams.Backend.Rasterific (
  renderRasterific, Rasterific)

import Linear (
  V2(V2))
import Linear.Affine (
  Point(P))
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
        False -> orange
        True -> blue
  let sampleDiagram = circle 0.02 & fillColor yColor
  return (P x, sampleDiagram))


main :: IO ()
main = do
  let whiteBackground =
        rect 2.1 2.1 & fillColor white & lineColor transparent
  let samplesDiagram =
        renderTrainingSamples trainingData `atop` whiteBackground
  let sizeSpec = dims (V2 600 600)
  renderRasterific "training_samples.png" sizeSpec samplesDiagram


