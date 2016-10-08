module Equipment.Widgets exposing
  ( addNewEquipBar
  , submitBtnGroup
  , formInput
  , deleteIcon
  )

{-| Common Html widgets used by various equipment panels. -}

import Html exposing (..)
import Html.Attributes as A
import Html.Events as E

import Types exposing (..)

{-| Widget to give the user the action to add a new equpment. -}
addNewEquipBar : String -> String -> String -> Msg -> Html Msg
addNewEquipBar color tooltip caption msg =
  div [ A.class color ]
      [ div [ A.class "w3-left" ]
          [ i [ A.class "material-icons w3-padding-tiny"
              , A.style [("cursor", "pointer")]
              , A.title tooltip
              , E.onClick msg
              ]
              [ text "add"]
          ]
      , h5 [] [ text caption ]
      ]

{-| Button group with two buttons - Submit and Cancel. -}
submitBtnGroup : Bool -> Msg -> Msg -> Html Msg
submitBtnGroup enabled submit cancel =
  div [ A.class "w3-container", A.style [("padding-bottom", "10px")]]
      [ button [ A.class "w3-btn w3-green"
               , A.disabled <| not enabled
               , E.onClick submit
               ]
               [ text "Submit" ]
      , button [ A.class "w3-btn w3-red"
               , E.onClick cancel
               ]
               [ text "Cancel" ]
      ]

formInput : String -> String -> (String -> Msg) -> Html Msg
formInput placeholder value action =
  input [ A.class "w3-input w3-light-grey"
        , A.type' "text"
        , A.placeholder placeholder
        , A.value value
        , E.onInput action
        ] []

{-| Icon, with action, to delete something. -}
deleteIcon : String -> Msg -> Html Msg
deleteIcon label action =
  i [ A.class "material-icons"
    , A.style [("cursor", "pointer")]
    , A.title <| "Delete " ++ label
    , E.onClick action
    ] [ text "delete" ]
