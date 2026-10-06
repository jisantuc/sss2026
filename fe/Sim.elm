module Sim exposing (defaultSimInput, timeToAnswerGround)

import Model exposing (ConfigState(..), DebugSimInput, NormalDist)
import Random
import StatRandom exposing (bernoulliBool, exponential, normal)


defaultSimInput : DebugSimInput
defaultSimInput =
    { lastIdeaAt = 0
    , startAfter = 0
    , ideaTime = 30
    , evidenceEnumerationMeanStd = { mean = 15, std = 6 }
    , evidenceEvaluationMeanStd = { mean = 15, std = 6 }
    , ideaCorrectRate = 20
    , chanceEvidenceOnTheGround = 100
    , timeTilContactMeanStd = { mean = 0, std = 0 }
    , retrievalSuccessRate = 100
    , configState = Frozen
    , color = { red = 20, green = 200, blue = 20 }
    }


timeToRetrieveEvidenceFromSatellite : NormalDist -> Float -> Random.Generator Float
timeToRetrieveEvidenceFromSatellite timeTilContact retrievalSuccessRate =
    Random.pair (normal timeTilContact.mean timeTilContact.std) (bernoulliBool (retrievalSuccessRate / 100))
        |> Random.andThen
            (\( delay, success ) ->
                if success then
                    Random.constant delay

                else
                    Random.map (\d -> delay + d) (timeToRetrieveEvidenceFromSatellite timeTilContact retrievalSuccessRate)
            )


timeToGatherEvidence : Float -> NormalDist -> Float -> Random.Generator Float
timeToGatherEvidence groundChance timeTilContact retrievalSuccessRate =
    bernoulliBool (groundChance / 100)
        |> Random.andThen
            (\onTheGround ->
                if onTheGround then
                    Random.constant 0

                else
                    timeToRetrieveEvidenceFromSatellite timeTilContact retrievalSuccessRate
            )


groundDebugTime : DebugSimInput -> Int -> Random.Generator Int
groundDebugTime { lastIdeaAt, startAfter, ideaTime, evidenceEnumerationMeanStd, evidenceEvaluationMeanStd, ideaCorrectRate, chanceEvidenceOnTheGround, timeTilContactMeanStd, retrievalSuccessRate, configState, color } timeout =
    let
        ideaDelayGen =
            exponential (1 / ideaTime) |> Random.map ceiling

        evidenceEnumerationDelayGen =
            normal evidenceEnumerationMeanStd.mean evidenceEnumerationMeanStd.std

        evidenceGatheringDelayGen =
            timeToGatherEvidence chanceEvidenceOnTheGround timeTilContactMeanStd retrievalSuccessRate

        evaluationDelayGen =
            normal evidenceEnumerationMeanStd.mean evidenceEnumerationMeanStd.std

        correctGen =
            bernoulliBool (ideaCorrectRate / 100.0)
    in
    Random.map5
        (\ideaDelay evidenceEnumerationDelay evidenceGatheringDelay evaluationDelay correct ->
            let
                ideaArrival =
                    lastIdeaAt + ideaDelay

                startAt =
                    max startAfter ideaArrival

                evidenceReadyAt =
                    startAt
                        + ceiling evidenceEnumerationDelay
                        + ceiling evidenceGatheringDelay

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

                else if lastResult >= timeout then
                    Random.constant timeout

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
                        , configState = configState
                        , color = color
                        }
                        timeout
            )


timeToAnswerGround : DebugSimInput -> Float -> Int -> Random.Generator Int
timeToAnswerGround groundDebugTimeParams chanceOfSoftwareBug timeout =
    bernoulliBool chanceOfSoftwareBug
        |> Random.andThen
            (\softwareBug ->
                if not softwareBug then
                    Random.constant timeout

                else
                    groundDebugTime groundDebugTimeParams timeout
            )
