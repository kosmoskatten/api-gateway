module Equipment.Widgets exposing
  ( addNewEquipBar
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
