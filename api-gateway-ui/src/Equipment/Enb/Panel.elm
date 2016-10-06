module Equipment.Enb.Panel exposing
  ( EnbModel
  , initEnb
  , viewEnbPanel
  , numEnbs
  )

{-| Viewing of, and handling the model of, the Enb control panel. -}

import Html exposing (..)
import Html.Attributes as A

import Types exposing (..)

{-| Model for the Enb panel. -}
type alias EnbModel =
  { enbs : List Enb
  }

{-| Initialize the Enb model. -}
initEnb : EnbModel
initEnb =
  { enbs = [] }

{-| View function for the Enb. -}
viewEnbPanel : EnbModel -> Html Msg
viewEnbPanel model =
  div [ A.class "w3-container" ]
    [ h4 [] [ text "ENBs" ]
    ]

{-| Tell the number of Enbs that are attached to the model. -}
numEnbs : EnbModel -> Int
numEnbs model = List.length model.enbs
