module Model exposing (ConfigState(..), DebugSimInput, Msg(..), NormalDist, SimState)

import Dict exposing (Dict)
import Element


type Msg
    = GeneratedValues Int (List Int)
    | RemoveConfig Int
    | CloneConfig Int
    | ToggleEditingConfig Int
    | EvidenceEnumerationMeanChanged Int Float
    | EvidenceEnumerationStdChanged Int Float
    | EvidenceEvaluationMeanChanged Int Float
    | EvidenceEvaluationStdChanged Int Float
    | TimeTilContactMeanChanged Int Float
    | TimeTilContactStdChanged Int Float
    | IdeaTimeChanged Int Float
    | IdeaAccuracyChanged Int Float
    | GroundChanceChanged Int Float
    | ContactSuccessRateChanged Int Float
    | NewRed Int Int
    | NewGreen Int Int
    | NewBlue Int Int
    | NoOp


type alias NormalDist =
    { mean : Float
    , std : Float
    }


type ConfigState
    = Editing
    | Frozen


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
    , configState : ConfigState
    , color : { red : Int, green : Int, blue : Int }
    }


type alias SimState =
    { nSamples : Int
    , configs : Dict Int DebugSimInput
    , simData : Dict Int (List Int)
    }
