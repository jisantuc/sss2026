{-# LANGUAGE FlexibleContexts #-}

module DebuggingTime where

import Control.Monad (replicateM)
import Control.Monad.Bayes.Class
  ( MonadDistribution,
    MonadFactor,
    bernoulli,
    condition,
    normal,
    poisson,
  )
import Control.Monad.Bayes.Sampler.Strict (SamplerT, sampler)
import Data.List (intersperse)
import System.Random.Stateful (IOGenM, StdGen)

timeToAnswerGround ::
  (MonadDistribution m, MonadFactor m) =>
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
  m Double
timeToAnswerGround lastIdeaAt startAfter ideaTime evidenceTime evaluationTime correctRate =
  do
    ideaDelay <- poisson ideaTime
    let ideaArrival = lastIdeaAt + ideaDelay
    let startAt = max startAfter (fromIntegral ideaArrival)
    evidenceEnumeration <- max 0 <$> uncurry normal evidenceTime
    condition $ evidenceEnumeration > 0
    let evidenceReadyAt = startAt + evidenceEnumeration
    evaluation <- max 0 <$> uncurry normal evaluationTime
    let resultAt = evidenceReadyAt + evaluation
    correct <- bernoulli correctRate
    if correct then pure resultAt else timeToAnswerGround ideaArrival resultAt ideaTime evidenceTime evaluationTime correctRate

logDebuggingTimes :: (Show a) => Int -> FilePath -> SamplerT (IOGenM StdGen) IO a -> IO ()
logDebuggingTimes nSamples dest generator =
  do
    samples <- sampler $ replicateM nSamples generator
    let dataLines = mconcat . intersperse "\n" $ show <$> samples
    writeFile dest ("debuggingTimes\n" <> dataLines)
