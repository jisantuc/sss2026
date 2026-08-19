module Main exposing (main)

import Browser
import Dict
import Model exposing (Msg(..), SimState)
import Random exposing (generate)
import Sim exposing (defaultSimInput, timeToAnswerGround)
import View exposing (view)


defaultSimState : SimState
defaultSimState =
    { nSamples = 1000, configs = [ ( 0, defaultSimInput ) ], simData = Dict.empty }


main : Program () SimState Msg
main =
    Browser.element
        { init = \_ -> ( defaultSimState, generate (GeneratedValues 0) (Random.list defaultSimState.nSamples (timeToAnswerGround defaultSimInput 1 2400)) )
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }



-- View goals:
-- - left hand side: render a plot of CDFs for each sim input
-- - right side: show sim inputs (to start!)
-- - right side: manage sim inputs (e.g. add new, remove)


update : Msg -> SimState -> ( SimState, Cmd Msg )
update msg model =
    case msg of
        GeneratedValues id values ->
            ( { model | simData = Dict.insert id values model.simData }, Cmd.none )
