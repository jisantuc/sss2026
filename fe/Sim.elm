module Sim exposing (defaultSimInput, timeToAnswerGround)

import Model exposing (DebugSimInput, NormalDist)
import Random
import StatRandom exposing (bernoulliBool, normal, poisson)


defaultSimInput : DebugSimInput
defaultSimInput =
    { lastIdeaAt = 0
    , startAfter = 0
    , ideaTime = 30
    , evidenceEnumerationMeanStd = { mean = 15, std = 6 }
    , evidenceEvaluationMeanStd = { mean = 15, std = 6 }
    , ideaCorrectRate = 0.2
    , chanceEvidenceOnTheGround = 1
    , timeTilContactMeanStd = { mean = 0, std = 0 }
    , retrievalSuccessRate = 1
    }


timeToRetrieveEvidenceFromSatellite : NormalDist -> Float -> Random.Generator Float
timeToRetrieveEvidenceFromSatellite timeTilContact retrievalSuccessRate =
    Random.pair (normal timeTilContact.mean timeTilContact.std) (bernoulliBool retrievalSuccessRate)
        |> Random.andThen
            (\( delay, success ) ->
                if success then
                    Random.constant delay

                else
                    Random.map (\d -> delay + d) (timeToRetrieveEvidenceFromSatellite timeTilContact retrievalSuccessRate)
            )


timeToGatherEvidence : Float -> NormalDist -> Float -> Random.Generator Float
timeToGatherEvidence groundChance timeTilContact retrievalSuccessRate =
    bernoulliBool groundChance
        |> Random.andThen
            (\onTheGround ->
                if onTheGround then
                    Random.constant 0

                else
                    timeToRetrieveEvidenceFromSatellite timeTilContact retrievalSuccessRate
            )


groundDebugTime : DebugSimInput -> Random.Generator Int
groundDebugTime { lastIdeaAt, startAfter, ideaTime, evidenceEnumerationMeanStd, evidenceEvaluationMeanStd, ideaCorrectRate, chanceEvidenceOnTheGround, timeTilContactMeanStd, retrievalSuccessRate } =
    let
        ideaDelayGen =
            poisson ideaTime 1

        evidenceEnumerationDelayGen =
            normal evidenceEnumerationMeanStd.mean evidenceEnumerationMeanStd.std

        evidenceGatheringDelayGen =
            timeToGatherEvidence chanceEvidenceOnTheGround timeTilContactMeanStd retrievalSuccessRate

        evaluationDelayGen =
            normal evidenceEnumerationMeanStd.mean evidenceEnumerationMeanStd.std

        correctGen =
            bernoulliBool ideaCorrectRate
    in
    Random.map5
        (\ideaDelay evidenceEnumerationDelay evidenceGatheringDelay evaluationDelay correct ->
            let
                ideaArrival =
                    lastIdeaAt + ideaDelay

                startAt =
                    max startAfter ideaArrival

                evidenceReadyAt =
                    startAt + ceiling evidenceEnumerationDelay + ceiling evidenceGatheringDelay

                resultAt =
                    evidenceReadyAt + ceiling evaluationDelay
            in
            ( ideaArrival, resultAt, correct )
        )
        ideaDelayGen
        evidenceEnumerationDelayGen
        evidenceGatheringDelayGen
        evaluationDelayGen
        correctGen
        |> Random.andThen
            (\( lastIdea, lastResult, correct ) ->
                if correct then
                    Random.constant lastResult

                else
                    groundDebugTime
                        { lastIdeaAt = lastIdea
                        , startAfter = lastResult
                        , ideaTime = ideaTime
                        , evidenceEnumerationMeanStd = evidenceEnumerationMeanStd
                        , evidenceEvaluationMeanStd = evidenceEvaluationMeanStd
                        , ideaCorrectRate = ideaCorrectRate
                        , chanceEvidenceOnTheGround = chanceEvidenceOnTheGround
                        , timeTilContactMeanStd = timeTilContactMeanStd
                        , retrievalSuccessRate = retrievalSuccessRate
                        }
            )


timeToAnswerGround : DebugSimInput -> Float -> Int -> Random.Generator Int
timeToAnswerGround groundDebugTimeParams chanceOfSoftwareBug timeout =
    bernoulliBool chanceOfSoftwareBug
        |> Random.andThen
            (\softwareBug ->
                if not softwareBug then
                    Random.constant timeout

                else
                    groundDebugTime groundDebugTimeParams
            )
