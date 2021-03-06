module Equipment.Ue.Panel exposing
  ( UeModel
  , initUe
  , viewUePanel
  , numUes
  , openNewUeForm
  , cancelNewUeForm
  , onInputNewUeImsi
  , newUeFormSubmitted
  , storedUesFetched
  , newUeCreated
  , ueDeleted
  )

{-| Viewing of, and handling the model of, the Ue control panel. -}

import Char exposing (isDigit)
import Html exposing (..)
import Html.Attributes as A
import List exposing (filter, map)
import Maybe exposing (withDefault)
import String exposing (all, length)

import Types exposing (..)
import Equipment.Widgets exposing ( addNewEquipBar, submitBtnGroup
                                  , formInput, deleteIcon
                                  )

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
    , viewUeList model
    ]

{-| Tell the number of Ues that are attached to the model. -}
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
    , submitBtnGroup (submitEnabled model.newUeImsi)
                     (SubmitNewUeForm model.newUeImsi) CancelNewUeForm
    ]

{-| View the list of Ues. -}
viewUeList : UeModel -> Html Msg
viewUeList model =
  table [ A.class "w3-table-all" ]
    (viewUeListHead :: map viewUeListItem model.ues)

viewUeListHead : Html Msg
viewUeListHead =
  tr []
    [ th [] [ text "UE IMSI" ]
    , th [] [ text "Cell PCI" ]
    , th [] [ text "Delete UE" ]
    ]

viewUeListItem : Ue -> Html Msg
viewUeListItem ue =
  tr []
    [ td [] [ text ue.imsi ]
    , td [] [ text <| pciAsString ue.pci ]
    , td [] [ deleteIcon ue.imsi (DeleteUe ue)]
    ]

pciAsString : Maybe Int -> String
pciAsString pci = withDefault "-" <| Maybe.map toString pci

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

{-| Response from the API, the Ue is created. -}
newUeCreated : UeModel -> Ue -> UeModel
newUeCreated model ue =
  {model | ues = model.ues ++ [ue] }

{-| Response from the API, the Ue is deleted. -}
ueDeleted : UeModel -> Ue -> UeModel
ueDeleted model ue =
  {model | ues = filter (\x -> x.imsi /= ue.imsi) model.ues}

{-| Input data validator, to tell if "Submit" shall be enabled. -}
submitEnabled : String -> Bool
submitEnabled newUe =
  length newUe > 0 && all isDigit newUe
