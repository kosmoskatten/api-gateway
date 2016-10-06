module Equipment.Enb.Panel exposing
  ( EnbModel
  , initEnb
  , viewEnbPanel
  , numEnbs
  , openNewEnbForm
  )

{-| Viewing of, and handling the model of, the Enb control panel. -}

import Html exposing (..)
import Html.Attributes as A

import Types exposing (..)
import Equipment.Widgets exposing ( addNewEquipBar )

{-| Model for the Enb panel. -}
type alias EnbModel =
  { newEnbFormOpen : Bool
  , enbs           : List Enb
  }

{-| Initialize the Enb model. -}
initEnb : EnbModel
initEnb =
  { newEnbFormOpen = False
  , enbs           = []
  }

{-| View function for the Enb. -}
viewEnbPanel : EnbModel -> Html Msg
viewEnbPanel model =
  div [ A.class "w3-container" ]
    [ h4 [] [ text "ENBs" ]
    , addNewEquipBar "w3-teal" "Open the form to create a new ENB"
                     "Add new ENB" OpenNewEnbForm
    ]

{-| Tell the number of Enbs that are attached to the model. -}
numEnbs : EnbModel -> Int
numEnbs model = List.length model.enbs

-- Event callsbacks from the main update function.

{-| Request to open the input form for creating a new ENB. -}
openNewEnbForm : EnbModel -> EnbModel
openNewEnbForm model =
  {model | newEnbFormOpen = True}
