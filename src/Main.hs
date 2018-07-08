module Main where

import Test.QuickCheck (
  Gen, oneof, choose)
import Test.QuickCheck.Random (
  mkQCGen)
import Test.QuickCheck.Gen (
  Gen(MkGen))
import Linear (
  V2(V2))
import Control.Monad (
  replicateM)


runGen :: Gen a -> a
runGen (MkGen gen) =
  gen (mkQCGen 489) 30

trainingData :: [(V2 Double, Bool)]
trainingData =
  runGen generateTrainingSamples

generateTrainingSamples :: Gen [(V2 Double, Bool)]
generateTrainingSamples =
  replicateM 50 generateTrainingSample

generateTrainingSample :: Gen (V2 Double, Bool)
generateTrainingSample = oneof [
  generateTrainingSample1,
  generateTrainingSample2]

generateTrainingSample1 :: Gen (V2 Double, Bool)
generateTrainingSample1 = do
  x1 <- choose (-1,0)
  x2 <- choose (-1,0)
  return (V2 x1 x2, False)

generateTrainingSample2 :: Gen (V2 Double, Bool)
generateTrainingSample2 = do
  x1 <- choose (0,1)
  x2 <- choose (0,1)
  return (V2 x1 x2, True)




main :: IO ()
main = do
  putStrLn (show trainingData)







