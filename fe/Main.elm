module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


type SimConfig
    = SimConfig
        { correctIdeaPercentage : Float,
          configId : Int
        }


defaultSimConfig : SimConfig
defaultSimConfig = SimConfig { correctIdeaPercentage = 0.2, configId = 0 }

main : Program () (List SimConfig) Msg
main =
    Browser.sandbox { init = [defaultSimConfig], update = update, view = view }

view _ = div [] [text "great!"]


type Msg
    = Increment
    | Decrement


update : Msg -> List SimConfig -> List SimConfig
update _ model =
        model
