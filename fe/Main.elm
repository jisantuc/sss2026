module Main exposing (main)

import Browser
import Html exposing (div, text)
import Sim exposing (DebugSimInput)


type SimConfig
    = SimConfig
        { nSamples : Int
        , configs : List DebugSimInput
        }


defaultSimConfig : SimConfig
defaultSimConfig =
    SimConfig { nSamples = 100, configs = [] }


main : Program () (List SimConfig) Msg
main =
    Browser.sandbox { init = [ defaultSimConfig ], update = update, view = view }



-- View goals:
-- - left hand side: render a plot of CDFs for each sim input
-- - right side: show sim inputs (to start!)
-- - right side: manage sim inputs (e.g. add new, remove)


view : a -> Html.Html msg
view _ =
    div [] [ text "great!" ]


type alias Msg =
    ()


update : Msg -> List SimConfig -> List SimConfig
update _ model =
    model
