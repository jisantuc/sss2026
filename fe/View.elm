module View exposing (view)

import Debug
import Dict
import Html exposing (div, text)
import List
import Model exposing (SimState)
import StatChart
import TypedSvg exposing (svg)


cdf : List Int -> List ( Float, Float )
cdf nums =
    if List.isEmpty nums then
        []

    else
        let
            sorted =
                List.sort nums

            length =
                toFloat (List.length sorted)
        in
        sorted
            |> List.indexedMap
                (\ix v ->
                    ( Debug.log (String.fromInt v) (toFloat v), toFloat ix / length )
                )


cdfGraph : List Int -> ( Float, Float, Float ) -> StatChart.Graph
cdfGraph d ( r, g, b ) =
    StatChart.graph StatChart.Line r g b (cdf d)


view : SimState -> Html.Html msg
view { nSamples, simData } =
    let
        emptyChart =
            { boundingBox = { xMin = -25, xMax = 2425, yMin = -0.05, yMax = 1.05 }, confidence = Nothing, data = [] }
    in
    div []
        [ div []
            [ text (String.concat [ "N samples: ", String.fromInt nSamples ])
            , text
                (String.concat
                    [ "Length of sim: ", List.length (Dict.get 0 simData |> Maybe.withDefault []) |> String.fromInt ]
                )
            ]
        , div []
            [ simData
                |> Dict.map
                    (\_ ser ->
                        cdfGraph ser ( 0.2, 0.2, 0.6 )
                    )
                |> Dict.values
                |> List.foldl StatChart.addGraph emptyChart
                |> StatChart.view { width = 2500, height = 100, padding = 36 } Nothing
                |> List.singleton
                |> svg []
            ]
        ]
