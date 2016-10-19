module Equipment.Enb.Panel exposing
  ( EnbModel
  , initEnb
  , viewEnbPanel
  , numEnbs
  , openNewEnbForm
  , cancelNewEnbForm
  , onInputNewEnb
  , newEnbFormSubmitted
  , newEnbCreated
  )

{-| Viewing of, and handling the model of, the Enb control panel. -}

import Char exposing (isDigit)
import Html exposing (..)
import Html.Attributes as A
import List exposing (map)
import String exposing (any, all, length)

import Char.Extra exposing (isSpace, isFirstCharAlpha)
import Types exposing (..)
import Equipment.Widgets exposing ( addNewEquipBar, submitBtnGroup
                                  , formInput
                                  )

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
    , viewEnbList model
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
    , div [ A.class "w3-container", A.style [("padding-bottom", "20px")] ]
      [ p [] []
      , label [] [ text "New ENB name" ]
      , formInput "Name for the new ENB (e.g. enb1)"
                  fields.newEnbName
                  (OnInputNewEnb (\f v -> {f | newEnbName = v}))
      , label [] [ text "New ENB ID" ]
      , formInput "Id for the new ENB (e.g. 1234)"
                  fields.newEnbId
                  (OnInputNewEnb (\f v -> {f | newEnbId = v}))
      , label [] [ text "New ENB MCC" ]
      , formInput "MCC for the new ENB (e.g. 234)"
                  fields.newEnbMcc
                  (OnInputNewEnb (\f v -> {f | newEnbMcc = v}))
      , label [] [ text "New ENB MNC" ]
      , formInput "MNC for the new ENB (e.g. 89)"
                  fields.newEnbMnc
                  (OnInputNewEnb (\f v -> {f | newEnbMnc = v}))
      , label [] [ text "New ENB MNC length" ]
      , formInput "MNC length for the new ENB (2 or 3, e.g. 2)"
                  fields.newEnbMncLength
                  (OnInputNewEnb (\f v -> {f | newEnbMncLength = v}))
      ]
    , submitBtnGroup (submitEnabled fields) (SubmitNewEnbForm fields)
                     CancelNewEnbForm
    ]

{-| View the list of ENBs. -}
viewEnbList : EnbModel -> Html Msg
viewEnbList model =
  table [ A.class "w3-table-all" ]
    (viewEnbListHead :: map viewEnbListItem model.enbs)

viewEnbListHead : Html Msg
viewEnbListHead =
  tr []
    [ th [] [ text "ENB name" ]
    , th [] [ text "Associated MME" ]
    , th [] [ text "Delete ENB" ]
    ]

viewEnbListItem : Enb -> Html Msg
viewEnbListItem enb =
  tr []
    [ td [] [text enb.name] ]

-- Event callbacks from the main update function.

{-| Request to open the input form for creating a new ENB. -}
openNewEnbForm : EnbModel -> EnbModel
openNewEnbForm model =
  {model | panelType = NewEnbForm emptyFormFields}

{-| Request to cancel the input form. -}
cancelNewEnbForm : EnbModel -> EnbModel
cancelNewEnbForm model =
  {model | panelType = AddEquip}

{-| New ENB name input. -}
onInputNewEnb : EnbModel -> (NewEnbFormFields -> String -> NewEnbFormFields)
             -> String -> EnbModel
onInputNewEnb model g value =
  case model.panelType of
    NewEnbForm fields ->
      { model | panelType = NewEnbForm <| g fields value }

    _                 -> model

{-| The form is submitted. -}
newEnbFormSubmitted : EnbModel -> EnbModel
newEnbFormSubmitted model =
  {model | panelType = AddEquip}

{-| Response for the API, the ENB is created. -}
newEnbCreated : EnbModel -> Enb -> EnbModel
newEnbCreated model enb =
  {model | enbs = model.enbs ++ [enb]}

submitEnabled : NewEnbFormFields -> Bool
submitEnabled fields =
  let nameOk   = length fields.newEnbName > 0
              && isFirstCharAlpha fields.newEnbName
              && not (any isSpace fields.newEnbName)

      idOk     = length fields.newEnbId > 0 && all isDigit fields.newEnbId

      mccOk    = length fields.newEnbMcc > 0 && all isDigit fields.newEnbMcc

      mncOk    = length fields.newEnbMnc > 0 && all isDigit fields.newEnbMnc

      mncLenOk = length fields.newEnbMncLength > 0
              && all isDigit fields.newEnbMncLength

  in nameOk && idOk && mccOk && mncOk && mncLenOk

emptyFormFields : NewEnbFormFields
emptyFormFields =
  { newEnbName      = ""
  , newEnbId        = ""
  , newEnbMcc       = ""
  , newEnbMnc       = ""
  , newEnbMncLength = ""
  }
