{-# LANGUAGE OverloadedStrings #-}

module DebuggingTime where

import Control.Monad (replicateM)
import Control.Monad.Bayes.Class
  ( MonadDistribution,
    bernoulli,
    normal,
    poisson,
  )
import Control.Monad.Bayes.Sampler.Strict (SamplerT, sampler)
import Data.List (inits, intersperse, sort)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (fromList)
import Graphics.Hgg.Backend.SVG (saveSVG)
import Graphics.Hgg.Spec
  ( ColRef (..),
    VisualSpec,
    alpha,
    axisMax,
    axisMin,
    color,
    layer,
    line,
    purePlot,
    rgb,
    title,
    xAxis,
    xLabel,
    yAxis,
    yLabel,
  )
import System.Random.Stateful (IOGenM, StdGen)

timeToGatherEvidence ::
  (MonadDistribution m) =>
  -- | Probability that the evidence is already on the ground
  Double ->
  -- | Time til next pass normal distribution params in minutes
  (Double, Double) ->
  m Double
timeToGatherEvidence groundChance timeTilContact = do
  onTheGround <- bernoulli groundChance
  if onTheGround
    then pure 0
    else
      uncurry normal timeTilContact

timeToAnswerGround ::
  (MonadDistribution m) =>
  -- | Time of last idea. The next idea arrives a random amount of time after this time.
  Int ->
  -- | The earliest you can start work on the next idea. Your debugging brain is single threaded,
  --        though ideas can arrive for processing at any time.
  Double ->
  -- | How many minutes it takes for the next idea to arrive. Poisson distributed around this value.
  Double ->
  -- | How many minutes it takes to gather evidence for the idea. Floored by 0.
  (Double, Double) ->
  -- | How many minutes it takes to evaluate the evidence for the idea. Floored by 0.
  (Double, Double) ->
  -- | How likely it is that each hypothesis is correct.
  Double ->
  -- | What's the chance that the evidence you need is on the ground already?
  Double ->
  -- | What's the distribution of times until next contact?
  (Double, Double) ->
  m Double
timeToAnswerGround lastIdeaAt startAfter ideaTime evidenceTime evaluationTime correctRate groundChance timeTilContact =
  do
    ideaDelay <- poisson ideaTime
    let ideaArrival = lastIdeaAt + ideaDelay
    let startAt = max startAfter (fromIntegral ideaArrival)
    evidenceEnumeration <- max 0 <$> uncurry normal evidenceTime
    evidenceGathering <- timeToGatherEvidence groundChance timeTilContact
    let evidenceReadyAt = startAt + evidenceEnumeration + evidenceGathering
    evaluation <- max 0 <$> uncurry normal evaluationTime
    let resultAt = evidenceReadyAt + evaluation
    correct <- bernoulli correctRate
    if correct
      then pure resultAt
      else
        timeToAnswerGround
          ideaArrival
          resultAt
          ideaTime
          evidenceTime
          evaluationTime
          correctRate
          groundChance
          timeTilContact

logDebuggingTimes :: (Show a) => Int -> FilePath -> SamplerT (IOGenM StdGen) IO a -> IO ()
logDebuggingTimes nSamples dest generator =
  do
    samples <- sampler $ replicateM nSamples generator
    let dataLines = mconcat . intersperse "\n" $ show <$> samples
    writeFile dest ("debuggingTimes\n" <> dataLines)

cdf :: (Num a, Ord a) => Double -> [a] -> [(Double, a)]
cdf _ [] = []
cdf percentileStep nums =
  let sorted = sort nums
      heads = inits sorted
      totalSize = fromIntegral $ length nums
      percentiles = drop 1 $ [0, percentileStep .. 1]
      indices = (-1 +) . floor . (* totalSize) <$> percentiles
   in zip percentiles $ last . (heads !!) <$> indices

plotCdf :: Text -> [Double] -> VisualSpec
plotCdf lab nums =
  let dist = cdf 0.01 nums
      percentiles = fst <$> dist
      values = snd <$> dist
   in purePlot
        <> ( layer $
               ( line (ColNum . fromList $ values) (ColNum . fromList $ percentiles)
                   <> (color $ rgb 61 65 79)
                   <> alpha 0.2
               )
           )
        <> yLabel "Proportion shorter than duration"
        <> xLabel "Debug duration"
        <> yAxis (axisMin 0)
        <> xAxis (axisMin 0 <> axisMax 1)
        <> title (Text.unwords ["CDF for ", lab])

saveDefaultCdf :: Int -> IO ()
saveDefaultCdf nSamples = do
  dist <- sampler . replicateM nSamples $ timeToAnswerGround 0 0 30 (15, 6) (15, 6) 0.2 1 (0, 0)
  let spec = plotCdf "Default Params" dist
  saveSVG "plots/debug-time-cdf-ground-default.svg" spec

saveBetterIdeasCdf :: Int -> IO ()
saveBetterIdeasCdf nSamples = do
  dist <- sampler . replicateM nSamples $ timeToAnswerGround 0 0 30 (15, 6) (15, 6) 0.4 1 (0, 0)
  let spec = plotCdf "Better Ideas" dist
  saveSVG "plots/debug-time-cdf-ground-better-ideas.svg" spec

saveFastIdeasCdf :: Int -> IO ()
saveFastIdeasCdf nSamples = do
  dist <- sampler . replicateM nSamples $ timeToAnswerGround 0 0 1 (15, 6) (15, 6) 0.2 1 (0, 0)
  let spec = plotCdf "Fast Ideas" dist
  saveSVG "plots/debug-time-cdf-ground-fast-ideas.svg" spec

saveSomeSpaceEvidenceCdf :: Int -> IO ()
saveSomeSpaceEvidenceCdf nSamples = do
  dist <- sampler . replicateM nSamples $ timeToAnswerGround 0 0 30 (15, 6) (15, 6) 0.2 0.75 (60, 15)
  let spec = plotCdf "Some Space Evidence" dist
  saveSVG "plots/debug-time-cdf-mixed-space-evidence.svg" spec
