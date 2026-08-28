module View exposing (view)

import Dict
import Element
import Element.Input as Input
import Html exposing (div)
import Html.Attributes exposing (style)
import List
import Model exposing (ConfigState(..), DebugSimInput, Msg(..), SimState)
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
                toFloat (List.length sorted)
        in
        sorted
            |> List.indexedMap
                (\ix v ->
                    ( toFloat v, toFloat ix / length )
                )
            |> uniquePercentiles
            |> List.reverse


cdfGraph : List Int -> ( Float, Float, Float ) -> StatChart.Graph
cdfGraph d ( r, g, b ) =
    StatChart.graph StatChart.Line r g b (cdf d)


viewConfig : ( Int, DebugSimInput ) -> Element.Element Msg
viewConfig ( ix, { configState } as conf ) =
    case configState of
        Frozen ->
            Element.column [ Element.padding 12, Element.spacing 12 ]
                [ Element.row [ Element.padding 12, Element.spacing 12 ]
                    [ Element.text ("Config id: " ++ String.fromInt ix)
                    , Input.button
                        [ Element.alignRight ]
                        { onPress = EditConfig ix |> Just
                        , label = Element.text "📝"
                        }
                    , Input.button
                        -- TODO: style the button
                        [ Element.alignRight ]
                        { onPress = RemoveConfig ix |> Just
                        , label = Element.text "🚮"
                        }
                    ]
                ]

        Editing ->
            Element.text "soon"

-- TODO: this is a table! This is for sure a table!
-- Don't hand roll this!
viewConfigs : List ( Int, DebugSimInput ) -> Element.Element Msg
viewConfigs configs =
    configs
        |> List.map viewConfig
        |> Element.column [ Element.alignTop, Element.width (Element.fillPortion 10) ]


view : SimState -> Html.Html Msg
view { nSamples, simData, configs } =
    let
        emptyChart =
            { boundingBox = { xMin = 0, xMax = 2500, yMin = 0, yMax = 1.05 }, confidence = Nothing, data = [] }
    in
    Element.row []
        [ [ div [ style "width" "100%" ]
                [ simData
                    |> Dict.map
                        (\_ ser ->
                            cdfGraph ser ( 0.2, 0.2, 0.6 )
                        )
                    |> Dict.values
                    |> List.foldl StatChart.addGraph emptyChart
                    |> StatChart.view { width = 800, height = 600, padding = 36 } Nothing
                ]
                |> Element.html
          , "N Samples: "
                ++ String.fromInt nSamples
                |> Element.text
                |> Element.el [ Element.centerX ]
          ]
            |> Element.column
                [ Element.width (Element.fillPortion 20 |> Element.minimum 800)
                , Element.height (Element.fill |> Element.minimum 800)
                ]
        , viewConfigs configs
        ]
        |> Element.layout [ Element.width Element.fill ]
