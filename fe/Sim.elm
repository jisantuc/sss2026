module Sim exposing (timeToAnswerGround)

import Random
import StatRandom exposing (bernoulliBool, normal, poisson)


type alias NormalDist =
    { mean : Float
    , std : Float
    }


type alias DebugSimInput =
    { lastIdeaAt : Int
    , startAfter : Int
    , ideaTime : Float
    , evidenceEnumerationMeanStd : NormalDist
    , evidenceEvaluationMeanStd : NormalDist
    , ideaCorrectRate : Float
    , chanceEvidenceOnTheGround : Float
    , timeTilContactMeanStd : NormalDist
    , retrievalSuccessRate : Float
    }


timeToGatherEvidence : Float -> Float -> Float -> Float -> Random.Generator Float
timeToGatherEvidence groundChance timeTilContactMean timeTilContactStd retrievalSuccessRate =
    Random.constant 0.3


groundDebugTime : DebugSimInput -> Random.Generator Int
groundDebugTime { lastIdeaAt, startAfter, ideaTime, evidenceEnumerationMeanStd, evidenceEvaluationMeanStd, ideaCorrectRate, chanceEvidenceOnTheGround, timeTilContactMeanStd, retrievalSuccessRate } =
    let
        ideaDelayGen =
            poisson ideaTime 1

        evidenceEnumerationDelayGen =
            normal evidenceEnumerationMeanStd.mean evidenceEnumerationMeanStd.std

        evidenceGatheringDelayGen =
            timeToGatherEvidence chanceEvidenceOnTheGround timeTilContactMeanStd.mean timeTilContactMeanStd.std retrievalSuccessRate

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
