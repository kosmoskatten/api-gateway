module Mme.Panel exposing
  ( MmeModel
  , initMme
  , viewMmePanel
  , numMmes
  , openNewMmeForm
  , cancelNewMmeForm
  , onInputNewMmeName
  , newMmeFormSubmitted
  , storedMmesFetched
  , newMmeCreated
  , mmeDeleted
  )

{-| Viewing of, and handling the model of, the Mme control panel. -}

import Array exposing (get)
import Char.Extra exposing (isAlpha, isSpace)
import Html exposing (..)
import Html.Attributes as A
import Html.Events as E
import List exposing (head, filter, map)
import Maybe exposing (withDefault)
import String exposing (length, toList, left, any)

import Types exposing (..)

{-| Model for the Mme panel. -}
type alias MmeModel =
  { newMmeFormOpen : Bool -- Flag to tell if the new mme form is open.
  , newMmeName     : String -- The value of the new mme form's input field.
  , mmes           : List Mme -- The set of Mmes attached to model.
  }

{-| Initialize the Mme model. -}
initMme : MmeModel
initMme =
  { newMmeFormOpen = False
  , newMmeName     = ""
  , mmes           = []
  }

{-| View function for the Mme. -}
viewMmePanel : MmeModel -> Html Msg
viewMmePanel model =
  div [ A.class "w3-container" ]
    [ h4 [] [ text "MMEs" ]
    , if model.newMmeFormOpen
        then newMmeForm model
        else addNewMme
    , viewMmeList model
    ]

{-| Tell the number of Mmes attached to the model. -}
numMmes : MmeModel -> Int
numMmes model = List.length model.mmes

{-| Widget to give the user the action to add a new Mme. -}
addNewMme : Html Msg
addNewMme =
  div [ A.class "w3-blue-grey" ]
      [ div [ A.class "w3-left" ]
          [ i [ A.class "material-icons w3-padding-tiny"
              , A.style [("cursor", "pointer")]
              , A.title "Open the form to create a new MME"
              , E.onClick OpenNewMmeForm
              ]
              [ text "add"]
          ]
      , h5 [] [ text "Add new MME" ]
      ]

{-| Widget to fill in the data for the Mme to be created. -}
newMmeForm : MmeModel -> Html Msg
newMmeForm model =
  div []
    [ div [ A.class "w3-container w3-blue-grey" ]
        [ h4 [] [ text "Add new MME"] ]
    , div [ A.class "w3-container", A.style [("padding-bottom", "20px")] ]
        [ p [] []
        , label [] [ text "New MME Name" ]
        , input [ A.class "w3-input w3-light-grey"
                , A.type' "text"
                , A.placeholder "Name for the new MME (e.g. mme1)"
                , A.value model.newMmeName
                , E.onInput OnInputNewMmeName
                ] []
        ]
    , div [ A.class "w3-container", A.style [("padding-bottom", "10px")]]
        [ button [ A.class "w3-btn w3-green"
                 , A.disabled (shallNewMmeSubmitBeDisabled model.newMmeName)
                 , E.onClick (SubmitNewMmeForm model.newMmeName)
                 ]
                 [ text "Submit" ]
        , button [ A.class "w3-btn w3-red"
                 , E.onClick CancelNewMmeForm
                 ]
                 [ text "Cancel" ]
        ]
    ]

{-| View the list of attached Mmes. -}
viewMmeList : MmeModel -> Html Msg
viewMmeList model =
  table [ A.class "w3-table-all" ]
    (map viewMmeListItem model.mmes)

viewMmeListItem : Mme -> Html Msg
viewMmeListItem mme =
  tr []
    [ td [] [ text mme.name ]
    , td [] [ text <| withDefault "-" (get 0 mme.addresses) ]
    , td []
        [ i [ A.class "material-icons"
            , A.style [("cursor", "pointer")]
            , A.title <| "Delete " ++ mme.name
            , E.onClick (DeleteMme mme)
            ]
            [ text "delete" ]
        ]
    ]

-- Event callbacks from the main update function.

{-| Request to open the input form for creating a new Mme. -}
openNewMmeForm : MmeModel -> MmeModel
openNewMmeForm model =
  {model | newMmeFormOpen = True}

{-| Request to cancel the input form. -}
cancelNewMmeForm : MmeModel -> MmeModel
cancelNewMmeForm model =
  {model | newMmeFormOpen = False
         , newMmeName     = ""
  }

{-| The user have entered text into the input field for the Mme's name. -}
onInputNewMmeName : MmeModel -> String -> MmeModel
onInputNewMmeName model newName =
  {model | newMmeName = newName}

{-| The user have pressed "Submit" for creating a new Mme. -}
newMmeFormSubmitted : MmeModel -> MmeModel
newMmeFormSubmitted model =
  {model | newMmeFormOpen = False
         , newMmeName     = ""
  }

{-| Response from the API, stored Mmes are fetched. -}
storedMmesFetched : MmeModel -> List Mme -> MmeModel
storedMmesFetched model mmes =
  {model | mmes = mmes }

{-| Response from the API, the Mme is created. -}
newMmeCreated : MmeModel -> Mme -> MmeModel
newMmeCreated model mme =
  {model | mmes = model.mmes ++ [mme]}

{-| Response from the API, the Mme is deleted. -}
mmeDeleted : MmeModel -> Mme -> MmeModel
mmeDeleted model mme =
  {model | mmes = filter (\x -> x.name /= mme.name) model.mmes}

{-| Input data validator, to tell if "Submit" shall be disabled. -}
shallNewMmeSubmitBeDisabled : String -> Bool
shallNewMmeSubmitBeDisabled newMme =
    length newMme < 1 || not (isFirstCharAlpha newMme) || any isSpace newMme

isFirstCharAlpha : String -> Bool
isFirstCharAlpha str =
  case firstChar str of
    Just c  -> isAlpha c
    Nothing -> False

firstChar : String -> Maybe Char
firstChar = head << toList << left 1
