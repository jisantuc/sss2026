module Model exposing (DebugSimInput, Msg(..), NormalDist, SimState)

import Dict exposing (Dict)


type Msg
    = GeneratedValues Int (List Int)


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


type alias SimState =
    { nSamples : Int
    , configs : List ( Int, DebugSimInput )
    , simData : Dict Int (List Int)
    }
