module View exposing (view)

import Debug
import Dict
import Html exposing (div, text)
import Html.Attributes exposing (style)
import List
import Model exposing (SimState)
import StatChart



uniquePercentiles : List ( float, float ) -> List ( float, float )
uniquePercentiles =
    List.foldl
        (\( v, percentile ) acc ->
            case List.head acc of
                Nothing ->
                    [ ( v, percentile ) ]

                Just ( h, _ ) ->
                    ( v, percentile )
                        :: (if h == v then
                                List.drop 1 acc

                            else
                                acc
                           )
        )
        []


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
                        ( toFloat v, toFloat ix / length )
                    )
                |> uniquePercentiles
                |> List.reverse
            )


cdfGraph : List Int -> ( Float, Float, Float ) -> StatChart.Graph
cdfGraph d ( r, g, b ) =
    StatChart.graph StatChart.Line r g b (Debug.log "incoming data" (cdf d))


view : SimState -> Html.Html msg
view { nSamples, simData } =
    let
        emptyChart =
            { boundingBox = { xMin = 0, xMax = 2500, yMin = 0, yMax = 1.05 }, confidence = Nothing, data = [] }
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
                |> Dict.map
                    (\_ ser ->
                        cdfGraph ser ( 0.2, 0.2, 0.6 )
                    )
                |> Dict.values
                |> List.foldl StatChart.addGraph emptyChart
                |> StatChart.view { width = 800, height = 600, padding = 36 } Nothing
            ]
        ]
