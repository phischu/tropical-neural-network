{-# LANGUAGE DeriveTraversable, GADTs #-}
module Main where

import Test.QuickCheck (
  Gen, choose, oneof, Arbitrary(arbitrary))
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
  Additive((^+^), (^-^)), V2(V2), V4(V4))
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
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

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
    let V2 p1 p2 = softArgMax (runNet net (fmap auto x))
    case c of
      False -> return (negate (log p1))
      True -> return (negate (log p2)))

softArgMax :: (Floating a) => V2 a -> V2 a
softArgMax x = let
  e = fmap exp x
  s = sum e
  in fmap (/s) e


-- Generate training data

data TrainingSample = TrainingSample {
  sampleInput :: V2 Double,
  sampleOutput :: Bool}
    deriving (Show)

type Rect a = V2 (V2 a)

runTrainingSampleGeneration :: V2 [Rect Double] -> [TrainingSample]
runTrainingSampleGeneration rects =
  runGen (replicateM 50 (generateTrainingSample rects))

generateTrainingSample :: V2 [Rect Double] -> Gen TrainingSample
generateTrainingSample (V2 falseRects trueRects)= do
  c <- arbitrary
  case c of
    False -> do
      x <- oneof (map generatePointInRect falseRects)
      return (TrainingSample x False)
    True -> do
      x <- oneof (map generatePointInRect trueRects)
      return (TrainingSample x True)

generatePointInRect :: Rect Double -> Gen (V2 Double)
generatePointInRect (V2 (V2 l1 l2) (V2 u1 u2)) = do
  x1 <- choose (l1, u1)
  x2 <- choose (l2, u2)
  return (V2 x1 x2)

generateRect :: Gen (Rect Double)
generateRect = do
  m1 <- choose (-0.8, 0.8)
  m2 <- choose (-0.8, 0.8)
  let m = V2 m1 m2
  s <- choose (0, 0.2)
  let r = V2 s s
  return (V2 (m ^-^ r) (m ^+^ r))

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
    opaque (blend (0.5 * (1 + c2 - c1)) lightOrange lightBlue)

  lightOrange = blend 0.2 white orange
  lightBlue = blend 0.2 white blue

  pointRaster i j = let
    x1 = 2 * (fromIntegral i / fromIntegral resolution) - 1
    x2 = negate (2 * (fromIntegral j / fromIntegral resolution) - 1)
    in V2 x1 x2

  pointColor i j =
    outputColor (softArgMax (runNet net (pointRaster i j)))

  in rasterDia pointColor resolution resolution & scaleUToX 2


numberOfOutputDiagrams :: Int
numberOfOutputDiagrams = 8

outputOnlyEvery :: Int
outputOnlyEvery = 100

outputDiagrams :: [TrainingSample] -> [Net Double] -> [Diagram Rasterific]
outputDiagrams trainingSamples exampleNets = take numberOfOutputDiagrams (do

  exampleNet <- every outputOnlyEvery exampleNets

  let outputDiagram = bgFrame 0.1 white (
        renderTrainingSamples trainingSamples `atop`
        renderNet exampleNet)

  return outputDiagram)

every :: Int -> [a] -> [a]
every n xs = case drop (n-1) xs of
  (y:ys) -> y : every n ys
  [] -> []


-- Main

runTraining :: [TrainingSample] -> [Net Double]
runTraining trainingSamples = let

  initialNet = runGen generateNet
  normalizedNet = fmap (/ sum initialNet) initialNet
  trainedNets = train trainingSamples normalizedNet

  in trainedNets


rects1 :: V2 [V2 (V2 Double)]
rects1 = V2
  [V2 (V2 (-1) (-1)) (V2 0 0)]
  [V2 (V2 0 0) (V2 1 1)]

rects2 :: V2 [V2 (V2 Double)]
rects2 = V2
  [V2 (V2 (-1) (-1)) (V2 0 0), V2 (V2 0 0) (V2 1 1)]
  [V2 (V2 (-1) 0) (V2 0 1), V2 (V2 0 (-1)) (V2 1 0)]

rects3 :: V2 [V2 ((V2 Double))]
rects3 = runGen (
  liftM2 V2 (replicateM 3 generateRect) (replicateM 3 generateRect))


main :: IO ()
main = do

  let trainingSamples1 = runTrainingSampleGeneration rects1
  let trainingSamples2 = runTrainingSampleGeneration rects2
  let trainingSamples3 = runTrainingSampleGeneration rects3

  let trainedNets1 = runTraining trainingSamples1
  let trainedNets2 = runTraining trainingSamples2
  let trainedNets3 = runTraining trainingSamples3

  let outputDiagrams1 = outputDiagrams trainingSamples1 trainedNets1
  let outputDiagrams2 = outputDiagrams trainingSamples2 trainedNets2
  let outputDiagrams3 = outputDiagrams trainingSamples3 trainedNets3

  let sizeSpec = dims (V2 600 600)

  renderRasterific "out/classification.png" sizeSpec (last outputDiagrams1)
  animatedGif "out/training1.gif" sizeSpec LoopingForever 100 outputDiagrams1
  animatedGif "out/training2.gif" sizeSpec LoopingForever 100 outputDiagrams2
  animatedGif "out/training3.gif" sizeSpec LoopingForever 100 outputDiagrams3


