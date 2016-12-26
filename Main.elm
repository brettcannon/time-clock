module Main exposing (..)

import Html
import Html.Attributes as HtmlAttr
import Html.Events
import Task
import Time


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


init : ( Model, Cmd Msg )
init =
    ( { workDayLength = 0
      , workLeft = 0
      , lunchLength = 0
      , lunchLeft = 0
      , ticking = False
      , outToLunch = False
      , lastTick = 0
      }
    , Cmd.none
    )


type alias Model =
    { workDayLength : Time.Time
    , workLeft : Time.Time
    , lunchLength : Time.Time
    , lunchLeft : Time.Time
    , ticking : Bool
    , outToLunch : Bool
    , lastTick : Time.Time
    }


-- UPDATE


type Msg
    = Start
    | Pause
    | FirstTick Time.Time
    | WorkTick Time.Time
    | ToggleLunch
    | LunchTick Time.Time
    | NewWorkLength String
    | NewLunchLength String


parseTimeLength : String -> Time.Time -> Time.Time
parseTimeLength timeString timeScale =
    timeScale * (Result.withDefault 0 <| String.toFloat timeString)


timeShift : Time.Time -> Time.Time -> Time.Time -> Time.Time
timeShift timeLeft oldLength newLength =
    let
        timeDiff =
            newLength - oldLength
    in
        timeLeft + timeDiff


timeDecrement : Time.Time -> Time.Time -> Time.Time -> Time.Time
timeDecrement timeLeft oldTime newTime =
    let
        timePast =
            newTime - oldTime
    in
        timeLeft - timePast


decrementWork : Model -> Time.Time -> Model
decrementWork model newTime =
    let
        newWorkLeft =
            timeDecrement model.workLeft model.lastTick newTime
    in
        { model | lastTick = newTime, workLeft = newWorkLeft }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model | ticking = True }, Task.perform FirstTick Time.now )

        FirstTick startTime ->
            ( { model | lastTick = startTime }, Cmd.none )

        Pause ->
            ( { model | ticking = False }, Task.perform WorkTick Time.now )

        ToggleLunch ->
            ( { model | outToLunch = not model.outToLunch }, Cmd.none )

        WorkTick newTime ->
            ( decrementWork model newTime
            , Cmd.none
            )

        LunchTick newTime ->
            let
                workDecrementedModel =
                    decrementWork model newTime

                lunchLeft =
                    timeDecrement model.lunchLeft model.lastTick newTime
            in
                ( { workDecrementedModel | lunchLeft = lunchLeft }
                , Cmd.none
                )

        NewWorkLength timeString ->
            let
                workLength =
                    parseTimeLength timeString Time.hour

                shiftedWorkLeft =
                    timeShift model.workLeft model.workDayLength workLength
            in
                ( { model
                    | workDayLength = workLength
                    , workLeft = shiftedWorkLeft
                  }
                , Cmd.none
                )

        NewLunchLength timeString ->
            let
                lunchLength =
                    parseTimeLength timeString Time.minute

                shiftedLunchLeft =
                    timeShift model.lunchLeft model.lunchLength lunchLength
            in
                ( { model
                    | lunchLength = lunchLength
                    , lunchLeft = shiftedLunchLeft
                  }
                , Cmd.none
                )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        ticker =
            Time.every Time.second
    in
        if model.ticking then
            if model.outToLunch then
                ticker LunchTick
            else
                ticker WorkTick
        else Sub.none


-- VIEW


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.label []
            [ Html.text "Length of workday (in hours) "
            , Html.input
                [ HtmlAttr.type_ "number"
                , HtmlAttr.min <| toString 0.0
                , HtmlAttr.max <| toString 24
                , HtmlAttr.step <| toString 0.1
                , HtmlAttr.value <| toString <| Time.inHours model.workDayLength
                , Html.Events.onInput NewWorkLength
                ]
                []
            ]
        , Html.br [] []
        , Html.label []
            [ Html.text "Length of lunch break (in minutes) "
            , Html.input
                [ HtmlAttr.type_ "number"
                , HtmlAttr.min <| toString 0
                , HtmlAttr.max <| toString (24 * 60)
                , HtmlAttr.step <| toString 1
                , HtmlAttr.value <| toString <| Time.inMinutes model.lunchLength
                , Html.Events.onInput NewLunchLength
                ]
                []
            ]
        , Html.br [] []
        , Html.span []
            [ Html.text "Seconds left in the workday: "
            , Html.text <| formatTime model.workLeft
            ]
        , Html.br [] []
        , Html.span []
            [ Html.text "Seonds left for lunch: "
            , Html.text <| formatTime model.lunchLeft
            ]
        , Html.br [] []
        , workButton model
        , Html.label []
            [ Html.input
                [ HtmlAttr.type_ "checkbox"
                , Html.Events.onClick ToggleLunch ] []
            , Html.text "Out to Lunch" ]
        ]


formatTime : Time.Time -> String
formatTime timeLeft =
    let
        leftInSeconds =
            round <| Time.inSeconds timeLeft

        hours =
            leftInSeconds // (60 * 60)

        leftAfterHours =
            leftInSeconds - (hours * 60 * 60)

        minutes =
            leftAfterHours // 60

        leftAfterMinutes =
            leftAfterHours - (minutes * 60)

        seconds =
            Time.inSeconds timeLeft
    in
        (if timeLeft < 0 then
            "-"
         else
            " "
        )
            ++ (String.pad 2 '0' <| toString <| hours)
            ++ ":"
            ++ (String.pad 2 '0' <| toString <| abs <| minutes)
            ++ ":"
            ++ (String.pad 2 '0' <| toString <| abs <| leftAfterMinutes)


workButton : Model -> Html.Html Msg
workButton model =
    Html.button
        [ Html.Events.onClick <|
            if not model.ticking then
                Start
            else
                Pause
        ]
        [ Html.text <|
            if not model.ticking then
                "Start"
            else
                "Pause"
        ]
