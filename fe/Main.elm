module Main exposing (main)

import Browser
import Dict
import Model exposing (ConfigState(..), Msg(..), SimState)
import Random exposing (generate)
import Sim exposing (defaultSimInput, timeToAnswerGround)
import View exposing (view)


defaultSimState : SimState
defaultSimState =
    { nSamples = 3000, configs = Dict.singleton 0 defaultSimInput, simData = Dict.empty }


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
    let
        regenerate ix conf =
            generate (GeneratedValues ix)
                (Random.list defaultSimState.nSamples (timeToAnswerGround conf 1 2400))

        regenerateFrom ix d =
            Dict.get ix d |> Maybe.map (regenerate ix) |> Maybe.withDefault Cmd.none
    in
    case msg of
        GeneratedValues id values ->
            ( { model | simData = Dict.insert id values model.simData }, Cmd.none )

        RemoveConfig configId ->
            let
                newConfigs =
                    model.configs |> Dict.remove configId

                newSimData =
                    model.simData |> Dict.remove configId
            in
            ( { model
                | configs =
                    if not (Dict.isEmpty newConfigs) then
                        newConfigs

                    else
                        model.configs
                , simData =
                    if not (Dict.isEmpty newSimData) then
                        newSimData

                    else
                        model.simData
              }
            , Cmd.none
            )

        ToggleEditingConfig configId ->
            ( { model
                | configs =
                    model.configs
                        |> Dict.update configId
                            (Maybe.map
                                (\conf ->
                                    { conf
                                        | configState =
                                            if conf.configState == Editing then
                                                Frozen

                                            else
                                                Editing
                                    }
                                )
                            )
              }
            , Cmd.none
            )

        EvidenceEnumerationMeanChanged ix new ->
            let
                newModel =
                    { model
                        | configs =
                            model.configs
                                |> Dict.update ix
                                    (Maybe.map
                                        (\conf ->
                                            { conf
                                                | evidenceEnumerationMeanStd =
                                                    { mean = new
                                                    , std = conf.evidenceEnumerationMeanStd.std
                                                    }
                                            }
                                        )
                                    )
                    }
            in
            ( newModel
            , regenerateFrom ix newModel.configs
            )

        EvidenceEnumerationStdChanged ix new ->
            let
                newModel =
                    { model
                        | configs =
                            model.configs
                                |> Dict.update ix
                                    (Maybe.map
                                        (\conf ->
                                            { conf
                                                | evidenceEnumerationMeanStd =
                                                    { mean = conf.evidenceEnumerationMeanStd.mean
                                                    , std = new
                                                    }
                                            }
                                        )
                                    )
                    }
            in
            ( newModel
            , regenerateFrom ix newModel.configs
            )

        EvidenceEvaluationMeanChanged ix new ->
            let
                newModel =
                    { model
                        | configs =
                            model.configs
                                |> Dict.update ix
                                    (Maybe.map
                                        (\conf ->
                                            { conf
                                                | evidenceEvaluationMeanStd =
                                                    { mean = new
                                                    , std = conf.evidenceEvaluationMeanStd.std
                                                    }
                                            }
                                        )
                                    )
                    }
            in
            ( newModel, regenerateFrom ix newModel.configs )

        EvidenceEvaluationStdChanged ix new ->
            let
                newModel =
                    { model
                        | configs =
                            model.configs
                                |> Dict.update ix
                                    (Maybe.map
                                        (\conf ->
                                            { conf
                                                | evidenceEvaluationMeanStd =
                                                    { mean = conf.evidenceEvaluationMeanStd.mean
                                                    , std = new
                                                    }
                                            }
                                        )
                                    )
                    }
            in
            ( newModel, regenerateFrom ix newModel.configs )

        TimeTilContactMeanChanged ix new ->
            let
                newModel =
                    { model
                        | configs =
                            model.configs
                                |> Dict.update ix
                                    (Maybe.map
                                        (\conf ->
                                            { conf
                                                | timeTilContactMeanStd =
                                                    { mean = new
                                                    , std = conf.timeTilContactMeanStd.std
                                                    }
                                            }
                                        )
                                    )
                    }
            in
            ( newModel, regenerateFrom ix newModel.configs )

        TimeTilContactStdChanged ix new ->
            let
                newModel =
                    { model
                        | configs =
                            model.configs
                                |> Dict.update ix
                                    (Maybe.map
                                        (\conf ->
                                            { conf
                                                | timeTilContactMeanStd =
                                                    { mean = conf.timeTilContactMeanStd.mean
                                                    , std = new
                                                    }
                                            }
                                        )
                                    )
                    }
            in
            ( newModel, regenerateFrom ix newModel.configs )

        IdeaTimeChanged ix new ->
            let
                newModel =
                    { model
                        | configs =
                            model.configs
                                |> Dict.update ix (Maybe.map (\conf -> { conf | ideaTime = new }))
                    }
            in
            ( newModel, regenerateFrom ix newModel.configs )

        IdeaAccuracyChanged ix new ->
            let
                newModel =
                    { model
                        | configs =
                            model.configs
                                |> Dict.update ix (Maybe.map (\conf -> { conf | ideaCorrectRate = new }))
                    }
            in
            ( newModel, regenerateFrom ix newModel.configs )

        GroundChanceChanged ix new ->
            let
                newModel =
                    { model
                        | configs =
                            model.configs
                                |> Dict.update ix (Maybe.map (\conf -> { conf | chanceEvidenceOnTheGround = new }))
                    }
            in
            ( newModel, regenerateFrom ix newModel.configs )

        ContactSuccessRateChanged ix new ->
            let
                newModel =
                    { model
                        | configs =
                            model.configs
                                |> Dict.update ix (Maybe.map (\conf -> { conf | retrievalSuccessRate = new }))
                    }
            in
            ( newModel, regenerateFrom ix newModel.configs )

        NewRed ix r ->
            let
                newModel =
                    { model
                        | configs =
                            model.configs
                                |> Dict.update ix
                                    (Maybe.map
                                        (\conf ->
                                            { conf
                                                | color =
                                                    { red = r
                                                    , green = conf.color.green
                                                    , blue = conf.color.blue
                                                    }
                                            }
                                        )
                                    )
                    }
            in
            ( newModel, Cmd.none )

        NewGreen ix g ->
            let
                newModel =
                    { model
                        | configs =
                            model.configs
                                |> Dict.update ix
                                    (Maybe.map
                                        (\conf ->
                                            { conf
                                                | color =
                                                    { red = conf.color.red
                                                    , green = g
                                                    , blue = conf.color.blue
                                                    }
                                            }
                                        )
                                    )
                    }
            in
            ( newModel, Cmd.none )

        NewBlue ix b ->
            let
                newModel =
                    { model
                        | configs =
                            model.configs
                                |> Dict.update ix
                                    (Maybe.map
                                        (\conf ->
                                            { conf
                                                | color =
                                                    { red = conf.color.red
                                                    , green = conf.color.green
                                                    , blue = b
                                                    }
                                            }
                                        )
                                    )
                    }
            in
            ( newModel, Cmd.none )

        CloneConfig ix ->
            let
                source =
                    Dict.get ix model.configs

                configIds =
                    Dict.keys model.configs

                nextId =
                    List.maximum configIds |> Maybe.map (\x -> x + 1) |> Maybe.withDefault 0

                newConfigs =
                    Maybe.map (\s -> Dict.insert nextId s model.configs) source
                        |> Maybe.withDefault model.configs

                cmd =
                    regenerateFrom nextId newConfigs
            in
            ( { model | configs = newConfigs }, cmd )

        NoOp ->
            ( model, Cmd.none )
