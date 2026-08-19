module View exposing (view)

import Debug
import Dict
import Html exposing (div, text)
import Html.Attributes exposing (style)
import List
import Model exposing (SimState)
import StatChart



-- TODO: lots of ties, have to deal with them, otherwise my line doesn't work
-- plain deduplication is wrong because each tie gets a different percentile
-- want something like nubBy + take last
cdf : List Int -> List ( Float, Float )
cdf nums =
    if List.isEmpty nums then
        []

    else
        let
            sorted =
                List.sort nums

            length =
                Debug.log "length of list" (toFloat (List.length sorted))
        in
        Debug.log "percentiles"
            (sorted
                |> List.indexedMap
                    (\ix v ->
                        ( toFloat v, toFloat ix / length)
                    )
            )


cdfGraph : List Int -> ( Float, Float, Float ) -> StatChart.Graph
cdfGraph d ( r, g, b ) =
    StatChart.graph StatChart.Line r g b (cdf d)


view : SimState -> Html.Html msg
view { nSamples, simData } =
    let
        emptyChart =
            { boundingBox = { xMin = 0, xMax = 2425, yMin = 0, yMax = 1.05 }, confidence = Nothing, data = [] }
    in
    div []
        [ div []
            [ text (String.concat [ "N samples: ", String.fromInt nSamples ])
            , text
                (String.concat
                    [ "Length of sim: ", List.length (Dict.get 0 simData |> Maybe.withDefault []) |> String.fromInt ]
                )
            ]
        , div [ style "width" "70%" ]
            [ simData
                -- TODO: maybe foldl instead of summoning a weird empty chart to start?
                |> Dict.map
                    (\_ ser ->
                        -- TODO: 
                        cdfGraph ser ( 0.2, 0.2, 0.6 )
                        -- graph StatChart.Line 0.2 0.2 0.6 [(1, 0.2), (100, 0.4), (700, 0.7), (1000, 0.8), (1, 0.4)]
                    )
                |> Dict.values
                |> List.foldl StatChart.addGraph emptyChart
                |> StatChart.view { width = 800, height = 600, padding = 36 } Nothing
            ]
        ]
