module Equipment.Enb.Panel exposing
  ( EnbModel
  , initEnb
  , viewEnbPanel
  , numEnbs
  , openNewEnbForm
  , cancelNewEnbForm
  , newEnbFormSubmitted
  )

{-| Viewing of, and handling the model of, the Enb control panel. -}

import Html exposing (..)
import Html.Attributes as A

import Types exposing (..)
import Equipment.Widgets exposing ( addNewEquipBar, submitBtnGroup )

{-| Model for the Enb panel. -}
type alias EnbModel =
  { panelType : PanelType
  , enbs      : List Enb
  }

type PanelType
  = AddEquip | NewEnbForm NewEnbFormFields

{-| Initialize the Enb model. -}
initEnb : EnbModel
initEnb =
  { panelType = AddEquip
  , enbs      = []
  }

{-| View function for the Enb. -}
viewEnbPanel : EnbModel -> Html Msg
viewEnbPanel model =
  div [ A.class "w3-container" ]
    [ h4 [] [ text "ENBs" ]
    , case model.panelType of
        AddEquip -> addNewEquipBar "w3-teal" "Open the form to create a new ENB"
                                   "Add new ENB" OpenNewEnbForm
        NewEnbForm fields -> newEnbForm fields
    ]

{-| Tell the number of Enbs that are attached to the model. -}
numEnbs : EnbModel -> Int
numEnbs model = List.length model.enbs

{-| Widget to fill in the data for the ENB to be created. -}
newEnbForm : NewEnbFormFields -> Html Msg
newEnbForm fields =
  div []
    [ div [ A.class "w3-container w3-teal" ]
        [ h4 [] [ text "Add new ENB "] ]
    , submitBtnGroup False (SubmitNewEnbForm fields) CancelNewEnbForm
    ]

-- Event callbacks from the main update function.

{-| Request to open the input form for creating a new ENB. -}
openNewEnbForm : EnbModel -> EnbModel
openNewEnbForm model =
  {model | panelType = NewEnbForm emptyFormFields}

{-| Request to cancel the input form. -}
cancelNewEnbForm : EnbModel -> EnbModel
cancelNewEnbForm model =
  {model | panelType = AddEquip}

{-| The form is submitted. -}
newEnbFormSubmitted : EnbModel -> EnbModel
newEnbFormSubmitted model =
  {model | panelType = AddEquip}

emptyFormFields : NewEnbFormFields
emptyFormFields =
  { newEnbName = ""
  }
