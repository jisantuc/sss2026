module View exposing (view)

import Dict
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html exposing (div)
import Html.Attributes exposing (style)
import List
import Model exposing (ConfigState(..), DebugSimInput, Msg(..), NormalDist, SimState)
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


configButtons : Int -> ConfigState -> Element.Element Msg
configButtons ix configState =
    Element.row []
        [ Input.button
            [ Element.alignRight ]
            { onPress = ToggleEditingConfig ix |> Just
            , label =
                (case configState of
                    Editing ->
                        "❌"

                    Frozen ->
                        "📝"
                )
                    |> Element.text
            }
        , Input.button
            [ Element.alignRight ]
            { onPress = RemoveConfig ix |> Just
            , label = Element.text "🚮"
            }
        , Input.button
            [ Element.alignRight ]
            { onPress = CloneConfig ix |> Just
            , label = Element.text "➕"
            }
        ]


rateConfigView : String -> Int -> ConfigState -> Float -> (Int -> Float -> Msg) -> Element.Element Msg
rateConfigView lab ix cs v msg =
    case cs of
        Editing ->
            Element.row [ Element.spaceEvenly ]
                [ Input.slider
                    [ Element.fill |> Element.minimum 90 |> Element.width
                    , Element.behindContent
                        (Element.el
                            [ Element.width Element.fill
                            , Element.height (Element.px 2)
                            , Element.centerY
                            , Background.color (Element.rgb 0.2 0.2 0.2)
                            , Border.rounded 2
                            ]
                            Element.none
                        )
                    ]
                    { onChange = \f -> msg ix f
                    , label = String.concat [ lab, "-", "floatval" ] |> Input.labelHidden
                    , min = 1
                    , max = 100
                    , step = Just 1
                    , value = v
                    , thumb = Input.defaultThumb
                    }
                , String.fromFloat v |> Element.text
                ]

        Frozen ->
            String.fromFloat v |> Element.text


floatConfigView : String -> Int -> ConfigState -> Float -> (Int -> Float -> Msg) -> Element.Element Msg
floatConfigView lab ix cs v msg =
    case cs of
        Editing ->
            Input.text [ Element.fill |> Element.minimum 30 |> Element.width ]
                { onChange =
                    \s ->
                        case String.toFloat s of
                            Just new ->
                                msg ix new

                            Nothing ->
                                NoOp
                , label = String.concat [ lab, "-", "floatval" ] |> Input.labelHidden
                , placeholder = Nothing
                , text = String.fromFloat v
                }

        Frozen ->
            String.fromFloat v |> Element.text


normalDistConfigView : String -> Int -> ConfigState -> NormalDist -> (Int -> Float -> Msg) -> (Int -> Float -> Msg) -> Element.Element Msg
normalDistConfigView lab ix cs { mean, std } meanChanged stdChanged =
    Element.row []
        [ Element.text "μ: "
        , mean
            |> (case cs of
                    Frozen ->
                        \v -> String.fromFloat v |> Element.text

                    Editing ->
                        \v ->
                            Input.text
                                [ Element.fill |> Element.minimum 30 |> Element.width ]
                                { onChange =
                                    \s ->
                                        case String.toFloat s of
                                            Just new ->
                                                meanChanged ix new

                                            Nothing ->
                                                NoOp
                                , label = String.concat [ lab, "-", "mean" ] |> Input.labelHidden
                                , placeholder = Nothing
                                , text = String.fromFloat v
                                }
               )
        , Element.text ", σ: "
        , std
            |> (case cs of
                    Frozen ->
                        \v -> String.fromFloat v |> Element.text

                    Editing ->
                        \v ->
                            Input.text []
                                { onChange =
                                    \s ->
                                        case String.toFloat s of
                                            Just new ->
                                                stdChanged ix new

                                            Nothing ->
                                                NoOp
                                , label = String.concat [ lab, "-", "std" ] |> Input.labelHidden
                                , placeholder = Nothing
                                , text = String.fromFloat v
                                }
               )
        ]


viewConfigs : List ( Int, DebugSimInput ) -> Element.Element Msg
viewConfigs configs =
    Element.table [ Element.padding 12, Element.spacing 12, Element.width (Element.fillPortion 20 |> Element.minimum 500), Element.alignLeft ]
        { data = configs
        , columns =
            [ { header = Element.text ""
              , width = Element.fill
              , view = \( configId, config ) -> configButtons configId config.configState
              }
            , { header = Element.text "Config id"
              , width = Element.fill
              , view = \( configId, _ ) -> String.fromInt configId |> Element.text
              }
            , { header = Element.text "Config"
              , width = Element.fill
              , view =
                    \( ix, config ) ->
                        Element.column [ Element.spacing 9 ]
                            [ Element.row [] [ Element.text "Idea arrival minutes: ", floatConfigView "idea-arrival" ix config.configState config.ideaTime IdeaTimeChanged ]
                            , Element.row [] [ Element.text "Idea accuracy: ", rateConfigView "idea-accuracy" ix config.configState config.ideaCorrectRate IdeaAccuracyChanged ]
                            , Element.row [] [ Element.text "Evidence enumeration: ", normalDistConfigView "enumeration" ix config.configState config.evidenceEnumerationMeanStd EvidenceEnumerationMeanChanged EvidenceEnumerationStdChanged ]
                            , Element.row [] [ Element.text "Evidence evaluation: ", normalDistConfigView "evaluation" ix config.configState config.evidenceEvaluationMeanStd EvidenceEvaluationMeanChanged EvidenceEvaluationStdChanged ]
                            , Element.row [] [ Element.text "Evidence on ground %: ", floatConfigView "ground-chance" ix config.configState config.chanceEvidenceOnTheGround GroundChanceChanged ]
                            , Element.row [] [ Element.text "Time til contact: ", normalDistConfigView "contact" ix config.configState config.timeTilContactMeanStd TimeTilContactMeanChanged TimeTilContactStdChanged ]
                            , Element.row [] [ Element.text "Contact success rate: ", rateConfigView "contact-success-rate" ix config.configState config.retrievalSuccessRate ContactSuccessRateChanged ]
                            ]
              }
            ]
        }


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
                [ Element.width (Element.fillPortion 60 |> Element.minimum 800)
                , Element.height (Element.fill |> Element.minimum 800)
                ]
        , viewConfigs (Dict.toList configs)
        ]
        |> Element.layout []
