module Equipment.Ue.Panel exposing
  ( UeModel
  , initUe
  , viewUePanel
  , numUes
  , openNewUeForm
  , cancelNewUeForm
  , onInputNewUeImsi
  , newUeFormSubmitted
  )

{-| Viewing of, and handling the model of, the Ue control panel. -}

import Char exposing (isDigit)
import Html exposing (..)
import Html.Attributes as A
import Html.Events as E
import String exposing (all, length)

import Types exposing (..)

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
        else addNewUe
    ]

{-| Tell the number of Ues attached to the model. -}
numUes : UeModel -> Int
numUes model = List.length model.ues

{-| Widget to give the user the action to a new Ue -}
addNewUe : Html Msg
addNewUe =
  div [ A.class "w3-blue" ]
      [ div [ A.class "w3-left" ]
          [ i [ A.class "material-icons w3-padding-tiny"
              , A.style [("cursor", "pointer")]
              , A.title "Open the form to create a new UE"
              , E.onClick OpenNewUeForm
              ]
              [ text "add" ]
          ]
      , h5 [] [ text "Add new UE" ]
      ]

{-| Widget to fill in the data for the Ue to be created. -}
newUeForm : UeModel -> Html Msg
newUeForm model =
  div []
    [ div [ A.class "w3-container w3-blue" ]
        [ h4 [] [ text "Add new UE"] ]
    , div [ A.class "w3-container", A.style [("padding-bottom", "20px")] ]
        [ p [] []
        , label [] [ text "New UE IMSI" ]
        , input [ A.class "w3-input w3-light-grey"
                , A.type' "text"
                , A.placeholder "Imsi number for the new UE (e.g. 123456)"
                , A.value model.newUeImsi
                , E.onInput OnInputNewUeImsi
                ] []
        ]
    , div [ A.class "w3-container", A.style [("padding-bottom", "10px")]]
        [ button [ A.class "w3-btn w3-green"
                 , A.disabled (shallNewUeSubmitBeDisabled model.newUeImsi)
                 , E.onClick (SubmitNewUeForm model.newUeImsi)
                 ]
                 [ text "Submit" ]
        , button [ A.class "w3-btn w3-red"
                 , E.onClick CancelNewUeForm
                 ]
                 [ text "Cancel" ]
        ]
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

{-| Input data validator, to tell if "Submit" shall be disabled. -}
shallNewUeSubmitBeDisabled : String -> Bool
shallNewUeSubmitBeDisabled newUe =
  length newUe < 1 || not (all isDigit newUe)