module Ue.Panel exposing
  ( UeModel
  , initUe
  , viewUePanel
  , numUes
  )

{- Viewing of, and handling the model of, the Ue control panel. -}

import Html exposing (..)
import Html.Attributes as A

import Types exposing (..)

{- Model for the Ue panel. -}
type alias UeModel =
  { ues : List Ue
  }

{- Initialize the Ue model. -}
initUe : UeModel
initUe = { ues = [] }

{- View function for the Ue. -}
viewUePanel : UeModel -> Html Msg
viewUePanel model =
  div [ A.class "w3-container" ]
    [ h4 [] [ text "The REAL UEs" ]
    ]

{- Tell the number of Ues attached to the model. -}
numUes : UeModel -> Int
numUes model = List.length model.ues
