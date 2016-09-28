module Equipment.Ue.Panel exposing
  ( UeModel
  , initUe
  , viewUePanel
  , numUes
  , openNewUeForm
  , cancelNewUeForm
  , onInputNewUeImsi
  , storedUesFetched
  , newUeFormSubmitted
  )

{-| Viewing of, and handling the model of, the Ue control panel. -}

import Char exposing (isDigit)
import Html exposing (..)
import Html.Attributes as A
import String exposing (all, length)

import Types exposing (..)
import Equipment.Widgets exposing (addNewEquipBar, submitBtnGroup, formInput)

{-| Model for the Ue panel. -}
type alias UeModel =
  { newUeFormOpen : Bool
  , newUeImsi     : String
  , ues           : List Ue
  }

{-| Initialize the Ue model. -}
initUe : UeModel
initUe =
  { newUeFormOpen = False
  , newUeImsi     = ""
  , ues           = []
  }

{-| View function for the Ue. -}
viewUePanel : UeModel -> Html Msg
viewUePanel model =
  div [ A.class "w3-container" ]
    [ h4 [] [ text "UEs" ]
    , if model.newUeFormOpen
        then newUeForm model
        else (addNewEquipBar "w3-blue" "Open the form to create a new UE"
                             "Add new UE" OpenNewUeForm)
    ]

{-| Tell the number of Ues attached to the model. -}
numUes : UeModel -> Int
numUes model = List.length model.ues

{-| Widget to fill in the data for the Ue to be created. -}
newUeForm : UeModel -> Html Msg
newUeForm model =
  div []
    [ div [ A.class "w3-container w3-blue" ]
        [ h4 [] [ text "Add new UE"] ]
    , div [ A.class "w3-container", A.style [("padding-bottom", "20px")] ]
        [ p [] []
        , label [] [ text "New UE IMSI" ]
        , formInput "Imsi number for the new UE (e.g. 123456)"
                    model.newUeImsi OnInputNewUeImsi
        ]
    , submitBtnGroup (shallNewUeSubmitBeDisabled model.newUeImsi)
                     (SubmitNewUeForm model.newUeImsi) CancelNewUeForm
    ]

-- Event callbacks from the main update function.

{-| Request to open the input form for creating a new UE. -}
openNewUeForm : UeModel -> UeModel
openNewUeForm model =
  {model | newUeFormOpen = True}

{-| Request to cancel the input form. -}
cancelNewUeForm : UeModel -> UeModel
cancelNewUeForm model =
  { model | newUeFormOpen = False
          , newUeImsi     = ""
  }

{-| The user have entered text into the input field for the Ue's imsi. -}
onInputNewUeImsi : UeModel -> String -> UeModel
onInputNewUeImsi model newImsi =
  {model | newUeImsi = newImsi}

{-| The user have pressed "Submit" for creating a new Ue. -}
newUeFormSubmitted : UeModel -> UeModel
newUeFormSubmitted model =
  { model | newUeFormOpen = False
          , newUeImsi     = ""
  }

{-| Response from the API, stored Ues are fetched. -}
storedUesFetched : UeModel -> List Ue -> UeModel
storedUesFetched model ues =
  {model | ues = ues }

{-| Input data validator, to tell if "Submit" shall be disabled. -}
shallNewUeSubmitBeDisabled : String -> Bool
shallNewUeSubmitBeDisabled newUe =
  length newUe < 1 || not (all isDigit newUe)
