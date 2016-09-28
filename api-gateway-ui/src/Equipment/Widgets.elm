module Equipment.Widgets exposing
  ( addNewEquipBar
  , submitBtnGroup
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
submitBtnGroup disable submit cancel =
  div [ A.class "w3-container", A.style [("padding-bottom", "10px")]]
      [ button [ A.class "w3-btn w3-green"
               , A.disabled disable
               , E.onClick submit
               ]
               [ text "Submit" ]
      , button [ A.class "w3-btn w3-red"
               , E.onClick cancel
               ]
               [ text "Cancel" ]
      ]
