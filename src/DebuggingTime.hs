{-# LANGUAGE LambdaCase #-}
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
import Data.Foldable (traverse_)
import Data.List (inits, intersperse, sort)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (fromList)
import Graphics.Hgg.Backend.SVG (saveSVG)
import Graphics.Hgg.Spec
  ( ColRef (..),
    Color,
    Layer,
    LineType (..),
    Point2 (Point2),
    VisualSpec,
    alpha,
    annotText,
    axisBreaksAt,
    color,
    layer,
    line,
    linePoints,
    linetype,
    purePlot,
    rgb,
    title,
    xAxis,
    xLabel,
    yAxis,
    yLabel,
  )
import System.Random.Stateful (IOGenM, StdGen)

defaultColor :: Color
defaultColor = rgb 61 65 79

lightBlue :: Color
lightBlue = rgb 0 60 192

newGreen :: Color
newGreen = rgb 0 128 60

timeToRetrieveEvidenceFromSatellite ::
  (MonadDistribution m) =>
  -- | Time til next pass normal distribution params in minutes
  (Double, Double) ->
  -- | Chance of success during each pass retrieving evidence from satellite
  Double ->
  m Double
timeToRetrieveEvidenceFromSatellite timeTilContact retrievalSuccessRate =
  do
    nextContactDelay <- uncurry normal timeTilContact
    successfulRetrieval <- bernoulli retrievalSuccessRate
    if successfulRetrieval
      then pure nextContactDelay
      else
        (nextContactDelay +) <$> timeToRetrieveEvidenceFromSatellite timeTilContact retrievalSuccessRate

timeToGatherEvidence ::
  (MonadDistribution m) =>
  -- | Probability that the evidence is already on the ground
  Double ->
  -- | Time til next pass normal distribution params in minutes
  (Double, Double) ->
  -- | Chance of success during each pass retrieving evidence from satellite
  Double ->
  m Double
timeToGatherEvidence groundChance timeTilContact retrievalSuccessRate = do
  onTheGround <- bernoulli groundChance
  if onTheGround
    then pure 0
    else
      timeToRetrieveEvidenceFromSatellite timeTilContact retrievalSuccessRate

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
  -- | How likely is it that each attempt to retrieve evidence from space succeeds?
  Double ->
  -- | What's the chance that it's a software bug at all?
  Double ->
  -- | How many minutes until you just give up?
  Double ->
  m Double
timeToAnswerGround
  lastIdeaAt
  startAfter
  ideaTime
  evidenceTime
  evaluationTime
  correctRate
  groundChance
  timeTilContact
  retrievalSuccessRate
  chanceOfSoftwareBug
  timeout =
    bernoulli chanceOfSoftwareBug >>= \case
      True ->
        do
          ideaDelay <- poisson ideaTime
          let ideaArrival = lastIdeaAt + ideaDelay
          let startAt = max startAfter (fromIntegral ideaArrival)
          evidenceEnumeration <- max 0 <$> uncurry normal evidenceTime
          evidenceGathering <- timeToGatherEvidence groundChance timeTilContact retrievalSuccessRate
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
                retrievalSuccessRate
                1
                timeout
      False -> pure timeout

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

cdfLayer :: Color -> [Double] -> Layer
cdfLayer c nums =
  let dist = cdf 0.01 nums
      percentiles = fst <$> dist
      values = snd <$> dist
   in line (ColNum . fromList $ values) (ColNum . fromList $ percentiles)
        <> alpha 0.2
        <> color c

oneWeekLine :: Layer
oneWeekLine = linePoints [Point2 2400 0, Point2 2400 1] <> linetype LtDashed <> color defaultColor

plotCdf :: Text -> [Layer] -> VisualSpec
plotCdf lab layers =
  purePlot
    <> (foldMap layer (oneWeekLine : layers))
    <> yLabel "Proportion shorter than duration"
    <> xLabel "Debug duration (minutes)"
    <> title (Text.unwords ["CDF for ", lab])
    <> xAxis (axisBreaksAt ((480 *) <$> [0, 1 .. 6]))
    <> yAxis (axisBreaksAt ([0, 0.2 .. 1] <> [1.05]))
    <> annotText 2200 0.5 "1 week"

refDist :: Int -> IO [Double]
refDist nSamples = sampler . replicateM nSamples $ timeToAnswerGround 0 0 30 (15, 6) (15, 6) 0.2 1 (0, 0) 1 1 2400

saveDefaultCdf :: Int -> IO ()
saveDefaultCdf nSamples = do
  dist <- refDist nSamples
  let spec = plotCdf "Default Params" [cdfLayer defaultColor dist]
  saveSVG "plots/debug-time-cdf-ground-default.svg" spec

saveBetterIdeasCdf :: Int -> IO ()
saveBetterIdeasCdf nSamples = do
  ref <- refDist nSamples
  dist <- sampler . replicateM nSamples $ timeToAnswerGround 0 0 30 (15, 6) (15, 6) 0.4 1 (0, 0) 1 1 2400
  let spec = plotCdf "Better Ideas" [cdfLayer defaultColor ref, cdfLayer newGreen dist]
  saveSVG "plots/debug-time-cdf-ground-better-ideas.svg" spec

saveWorseIdeasCdf :: Int -> IO ()
saveWorseIdeasCdf nSamples = do
  ref <- refDist nSamples
  dist <- sampler . replicateM nSamples $ timeToAnswerGround 0 0 30 (15, 6) (15, 6) 0.1 1 (0, 0) 1 1 2400
  let spec = plotCdf "Worse Ideas" [cdfLayer defaultColor ref, cdfLayer newGreen dist]
  saveSVG "plots/debug-time-cdf-ground-worse-ideas.svg" spec

saveFastIdeasCdf :: Int -> IO ()
saveFastIdeasCdf nSamples = do
  ref <- refDist nSamples
  dist <- sampler . replicateM nSamples $ timeToAnswerGround 0 0 1 (15, 6) (15, 6) 0.2 1 (0, 0) 1 1 2400
  let spec = plotCdf "Fast Ideas" [cdfLayer defaultColor ref, cdfLayer newGreen dist]
  saveSVG "plots/debug-time-cdf-ground-fast-ideas.svg" spec

saveSomeSpaceEvidenceCdf :: Int -> IO ()
saveSomeSpaceEvidenceCdf nSamples = do
  ref <- refDist nSamples
  dist <- sampler . replicateM nSamples $ timeToAnswerGround 0 0 30 (15, 6) (15, 6) 0.2 0.75 (60, 15) 1 1 2400
  let spec = plotCdf "Some Space Evidence" [cdfLayer defaultColor ref, cdfLayer newGreen dist]
  saveSVG "plots/debug-time-cdf-mixed-space-evidence.svg" spec

saveSomeSpaceEvidenceSomePassFailuresCdf :: Int -> IO ()
saveSomeSpaceEvidenceSomePassFailuresCdf nSamples = do
  ref <- refDist nSamples
  dist <- sampler . replicateM nSamples $ timeToAnswerGround 0 0 30 (15, 6) (15, 6) 0.2 0.75 (60, 15) 0.9 1 2400
  let spec =
        plotCdf
          "Some Space Evidence with Some Pass Failures"
          [ cdfLayer defaultColor ref,
            cdfLayer newGreen dist
          ]
  saveSVG "plots/debug-time-cdf-mixed-space-evidence-pass-failures.svg" spec

saveMaybeNotSoftwareBugCdf :: Int -> IO ()
saveMaybeNotSoftwareBugCdf nSamples = do
  ref <- refDist nSamples
  dist <- sampler . replicateM nSamples $ timeToAnswerGround 0 0 30 (15, 6) (15, 6) 0.2 0.75 (60, 15) 0.9 0.9 2400
  let spec = plotCdf "Some Chance of Not a Software Bug" [cdfLayer defaultColor ref, cdfLayer newGreen dist]
  saveSVG "plots/debug-time-maybe-not-software-bug.svg" spec

saveMaybeNotSoftwareBugDumber :: Int -> IO ()
saveMaybeNotSoftwareBugDumber nSamples = do
  ref <- refDist nSamples
  dist <-
    sampler . replicateM nSamples $
      timeToAnswerGround 0 0 30 (15 * 1.25, 6) (15 * 1.25, 6) (0.2 * 0.75) 0.75 (60, 15) 0.9 0.9 2400
  let spec = plotCdf "Debug time with 25% worse understanding" [cdfLayer defaultColor ref, cdfLayer newGreen dist]
  saveSVG "plots/debug-time-ref-vs-worst-case.svg" spec

saveMaybeNotSoftwareBugTwoRefs :: Int -> IO ()
saveMaybeNotSoftwareBugTwoRefs nSamples = do
  midRef <- sampler . replicateM nSamples $ timeToAnswerGround 0 0 30 (15, 6) (15, 6) 0.2 0.75 (60, 15) 0.9 1 2400
  dist <- sampler . replicateM nSamples $ timeToAnswerGround 0 0 30 (15, 6) (15, 6) 0.2 0.75 (60, 15) 0.9 0.9 2400
  let spec =
        plotCdf
          "Some Chance of Not a Software Bug"
          [ cdfLayer lightBlue midRef <> linetype LtDashed,
            cdfLayer newGreen dist,
            linePoints [Point2 0 0, Point2 2400 0, Point2 2400 1] <> color lightBlue <> linetype LtDashed,
            linePoints [Point2 480 0.675, Point2 480 0.9] <> linetype LtDashed <> color defaultColor,
            linePoints [Point2 720 0.75, Point2 720 0.975] <> linetype LtDashed <> color defaultColor,
            linePoints [Point2 960 0.775, Point2 960 1] <> linetype LtDashed <> color defaultColor
          ]
  saveSVG "plots/debug-time-maybe-not-software-bug-two-references.svg" spec

saveMaybeNotSoftwareFewerSoftwareBugsTwoRefs :: Int -> IO ()
saveMaybeNotSoftwareFewerSoftwareBugsTwoRefs nSamples = do
  midRef <- sampler . replicateM nSamples $ timeToAnswerGround 0 0 30 (15, 6) (15, 6) 0.2 0.75 (60, 15) 0.9 1 2400
  dist <- sampler . replicateM nSamples $ timeToAnswerGround 0 0 30 (15, 6) (15, 6) 0.2 0.75 (60, 15) 0.9 0.7 2400
  let spec =
        plotCdf
          "Some Chance of Not a Software Bug"
          [ cdfLayer lightBlue midRef <> linetype LtDashed,
            cdfLayer newGreen dist,
            linePoints [Point2 0 0, Point2 2400 0, Point2 2400 1] <> color lightBlue <> linetype LtDashed,
            linePoints [Point2 480 0.5, Point2 480 0.9] <> linetype LtDashed <> color defaultColor,
            linePoints [Point2 720 0.6, Point2 720 0.975] <> linetype LtDashed <> color defaultColor,
            linePoints [Point2 960 0.625, Point2 960 1] <> linetype LtDashed <> color defaultColor
          ]
  saveSVG "plots/debug-time-maybe-not-software-bug-fewer-ground-defects.svg" spec

allPlots :: Int -> IO ()
allPlots nSamples =
  traverse_
    ($ nSamples)
    [ saveDefaultCdf,
      saveWorseIdeasCdf,
      saveBetterIdeasCdf,
      saveFastIdeasCdf,
      saveSomeSpaceEvidenceCdf,
      saveSomeSpaceEvidenceSomePassFailuresCdf,
      saveMaybeNotSoftwareBugCdf,
      saveMaybeNotSoftwareBugTwoRefs,
      saveMaybeNotSoftwareFewerSoftwareBugsTwoRefs,
      saveMaybeNotSoftwareBugDumber
    ]
