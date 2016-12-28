module Mdl exposing (..)
import Html
import Html.Attributes as HtmlAttr
import Html.Events

grid : List (Html.Html a) -> Html.Html a
grid contents =
    Html.div [ HtmlAttr.class "mdl-grid" ] contents


cell : Int -> List (Html.Html a) -> Html.Html a
cell columns contents =
    Html.div
        [ HtmlAttr.class ("mdl-cell mdl-cell--" ++ toString columns ++ "-col") ]
        contents


checkbox : String -> a -> Html.Html a
checkbox label onClickMsg =
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


textField : String -> String -> (String -> a) -> List (Html.Attribute a) -> Html.Html a
textField type_ label onInputMsg attrs =
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


badge : String -> String -> Html.Html a
badge message contents =
    Html.span
        [HtmlAttr.class "mdl-badge", HtmlAttr.attribute "data-badge" message ]
        [ Html.text contents ]
