{-# LANGUAGE DeriveTraversable, GADTs #-}
module Main where

import Test.QuickCheck (
  Gen, oneof, choose, Arbitrary(arbitrary))
import Test.QuickCheck.Random (
  mkQCGen)
import Test.QuickCheck.Gen (
  Gen(MkGen))

import Diagrams.Prelude (
  Diagram, circle, position, rasterDia,
  atop, scaleUToX,
  fillColor, orange, blue, white, opaque, blend,
  bgFrame,
  dims)
import Diagrams.Backend.Rasterific (
  Rasterific, renderRasterific,
  animatedGif, GifLooping(LoopingForever))

import Numeric.AD (
  gradientDescent, auto, Mode, Scalar)

import Linear (
  Additive, V2(V2), V4(V4))
import Linear.Affine (
  Point(P))
import Linear.Matrix (
  M24, M42, (!*))

import Control.Monad (
  replicateM, liftM2, liftM4)
import Data.Function (
  (&))


-- Net

data Net a = Net (M24 a) (M42 a)
  deriving (Functor, Foldable, Traversable)

runNet :: (Fractional a, Ord a) => Net a -> V2 a -> V2 a
runNet (Net m1 m0) = tropically m1 . normally m0

normally :: (Functor v, Foldable w, Additive w, Num a) =>
  v (w a) -> w a -> v a
normally matrix vector =
  matrix !* vector

tropically :: (Functor v, Foldable w, Additive w, Ord a, Num a) =>
  v (w a) -> w a -> v a
tropically matrix vector =
  fmap unTropical (fmap (fmap Tropical) matrix !* fmap Tropical vector)

-- Training

train :: [TrainingSample] -> Net Double -> [Net Double]
train trainingSamples net =
  gradientDescent (lossFunction trainingSamples) net

lossFunction ::
  (Num a, Fractional a, Floating a, Ord a, Mode a, Scalar a ~ Double) =>
  [TrainingSample] -> Net a -> a
lossFunction trainingSamples net = sum (do
    TrainingSample x c <- trainingSamples
    let V2 c1 c2 = runNet net (fmap auto x)
    case c of
      False -> return (negate (log c1))
      True -> return (negate (log c2)))


-- Generate training data

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


-- Tropical

newtype Tropical a = Tropical { unTropical :: a }
  deriving (Functor, Foldable, Traversable)

instance (Num a, Ord a) => Num (Tropical a) where
  (Tropical a) + (Tropical b) = Tropical (a `max` b)
  (Tropical a) * (Tropical b) = Tropical (a + b)
  (Tropical _) - (Tropical _) = error "Tropical: unsupported operation"
  negate (Tropical _) = error "Tropical: unsupported operation"
  abs (Tropical _) = error "Tropical: unsupported operation"
  signum (Tropical _) = error "Tropical: unsupported operation"
  fromInteger a = Tropical (fromInteger a)

instance (Arbitrary a) => Arbitrary (Tropical a) where
  arbitrary = fmap Tropical arbitrary


-- Generate initial Net

generateNet :: (Arbitrary a) => Gen (Net a)
generateNet = liftM2 Net generateM24 generateM42

generateM42 :: (Arbitrary a) => Gen (M42 a)
generateM42 = liftM4 V4 generateV2 generateV2 generateV2 generateV2

generateM24 :: (Arbitrary a) => Gen (M24 a)
generateM24 = liftM2 V2 generateV4 generateV4

generateV2 :: (Arbitrary a) => Gen (V2 a)
generateV2 = liftM2 V2 arbitrary arbitrary

generateV4 :: (Arbitrary a) => Gen (V4 a)
generateV4 = liftM4 V4 arbitrary arbitrary arbitrary arbitrary


exampleNets :: [Net Double]
exampleNets = let
  initialNet = runGen generateNet
  trainedNets = train trainingData initialNet
  in trainedNets


-- Visualization

renderTrainingSamples :: [TrainingSample] -> Diagram Rasterific
renderTrainingSamples trainingSamples = position (do
  TrainingSample x y <- trainingSamples
  let yColor = case y of
        False -> blue
        True -> orange
  let sampleDiagram = circle 0.02 & fillColor yColor
  return (P x, sampleDiagram))


renderNet :: Net Double -> Diagram Rasterific
renderNet net = let

  resolution = 600

  outputColor (V2 c1 c2) =
    opaque (blend (logistic (c2 - c1)) lightOrange lightBlue)
  logistic x = recip (1 + exp (negate x))
  lightOrange = blend 0.2 white orange
  lightBlue = blend 0.2 white blue

  pointRaster i j = let
    x1 = 2 * (fromIntegral i / fromIntegral resolution) - 1
    x2 = negate (2 * (fromIntegral j / fromIntegral resolution) - 1)
    in V2 x1 x2

  pointColor i j =
    outputColor (runNet net (pointRaster i j))

  in rasterDia pointColor resolution resolution & scaleUToX 2


main :: IO ()
main = do
  let numberOfOutputDiagrams = 4
  let outputOnlyEvery = 100
  let outputDiagrams = take numberOfOutputDiagrams (do
        exampleNet <- every outputOnlyEvery exampleNets
        return (
          bgFrame 0.1 white (
          renderTrainingSamples trainingData `atop`
          renderNet exampleNet)))
  let sizeSpec =
        dims (V2 600 600)
  renderRasterific "out/classification.png" sizeSpec (last outputDiagrams)
  animatedGif "out/training.gif" sizeSpec LoopingForever 100 outputDiagrams


every :: Int -> [a] -> [a]
every n xs = case drop (n-1) xs of
  (y:ys) -> y : every n ys
  [] -> []

