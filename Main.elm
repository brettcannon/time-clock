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
        else
            Sub.none



-- VIEW


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ mdlGrid
            [ mdlCell 3
                [ mdlTextField "number"
                    "Hours in the workday ..."
                    NewWorkLength
                    [ HtmlAttr.min <| toString 0.0
                    , HtmlAttr.max <| toString 24
                    , HtmlAttr.step <| toString 0.1
                    ]
                ]
            , mdlCell 3
                [ mdlTextField "number"
                    "Minutes for a lunch break ..."
                    NewLunchLength
                    [ HtmlAttr.min <| toString 0.0
                    , HtmlAttr.max <| toString (24 * 60)
                    , HtmlAttr.step <| toString 1
                    ]
                ]
            ]
        , mdlGrid
            [ mdlCell 12
                [ workButton model ]
            ]
        , mdlGrid
            [ mdlCell 12
                [ Html.hr [] []
                , Html.h1 [] (formatTime "👩\x200D💻" model.workLeft)
                ]
            ]
        , mdlGrid
            [ mdlCell 12
                [ Html.h1 [] (formatTime "🍱" model.lunchLeft)
                , Html.hr [] []
                ]
            ]
        , mdlGrid
            [ mdlCell 12
                [ mdlCheckbox "Out to Lunch" ToggleLunch ]
            ]
        ]


mdlGrid : List (Html.Html Msg) -> Html.Html Msg
mdlGrid contents =
    Html.div [ HtmlAttr.class "mdl-grid" ] contents


mdlCell : Int -> List (Html.Html Msg) -> Html.Html Msg
mdlCell columns contents =
    Html.div
        [ HtmlAttr.class ("mdl-cell mdl-cell--" ++ toString columns ++ "-col") ]
        contents


mdlCheckbox : String -> Msg -> Html.Html Msg
mdlCheckbox label onClickMsg =
    Html.label
        [ HtmlAttr.class "mdl-checkbox mdl-js-checkbox mdl-js-ripple-effect" ]
        [ Html.input
            [ HtmlAttr.type_ "checkbox"
            , Html.Events.onClick onClickMsg
            , HtmlAttr.class "mdl-checkbox__input"
            ]
            []
        , Html.span
            [ HtmlAttr.class "mdl-checkbox__label" ]
            [ Html.text label ]
        ]


mdlTextField : String -> String -> (String -> Msg) -> List (Html.Attribute Msg) -> Html.Html Msg
mdlTextField type_ label onInputMsg attrs =
    Html.div
        [ HtmlAttr.class "mdl-textfield mdl-js-textfield mdl-textfield--floating-label" ]
        [ Html.input
            ([ HtmlAttr.class "mdl-textfield__input"
             , HtmlAttr.type_ type_
             , Html.Events.onInput onInputMsg
             ]
                ++ attrs
            )
            []
        , Html.label
            [ HtmlAttr.class "mdl-textfield__label" ]
            [ Html.text label ]
        ]


mdlBadge : String -> String -> Html.Html Msg
mdlBadge message contents =
    Html.span
        [HtmlAttr.class "mdl-badge", HtmlAttr.attribute "data-badge" message ]
        [ Html.text contents ]


formatTime : String -> Time.Time -> List(Html.Html Msg)
formatTime suffix timeLeft =
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

        timeLeftString =
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
    in
        if timeLeft >= 0 then
            [ Html.span [] [ Html.text <| timeLeftString ++ " " ++ suffix ] ]
        else
            [ mdlBadge "!" timeLeftString
            , Html.text <| suffix
            ]


workButton : Model -> Html.Html Msg
workButton model =
    Html.button
        [ Html.Events.onClick <|
            if not model.ticking then
                Start
            else
                Pause
        , HtmlAttr.class
            "mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect mdl-button--accent"
        ]
        [ Html.i [ HtmlAttr.class "material-icons" ]
            [ Html.text <|
                if not model.ticking then
                    "play_arrow"
                else
                    "pause"
            ]
        ]
