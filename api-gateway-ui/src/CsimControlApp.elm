module CsimControlApp exposing
  ( init
  , view
  , update
  , subscriptions
  )

{-| Top controlling module for the app. Provides the top level layout
    and the app's message pump.
-}

import Html exposing (..)
import Html.Attributes as A
import Html.Events as E
import HttpBuilder exposing (Error (..), Response)
import Unicode as Uni

import Types exposing (..)

import Equipment.Enb.Panel exposing ( EnbModel, initEnb, numEnbs
                                    , viewEnbPanel, openNewEnbForm
                                    , cancelNewEnbForm, onInputNewEnb
                                    , newEnbFormSubmitted, newEnbCreated
                                    )
import Equipment.Enb.Rest exposing (createEnb)
import Equipment.Mme.Panel exposing ( MmeModel, initMme, numMmes
                                    , viewMmePanel, openNewMmeForm
                                    , cancelNewMmeForm, onInputNewMmeName
                                    , newMmeFormSubmitted, storedMmesFetched
                                    , newMmeCreated, mmeDeleted
                                    )
import Equipment.Mme.Rest exposing (createMme, deleteMme, fetchStoredMmes)
import Equipment.Ue.Panel exposing ( UeModel, initUe, numUes
                                   , viewUePanel, openNewUeForm
                                   , cancelNewUeForm, onInputNewUeImsi
                                   , newUeFormSubmitted, storedUesFetched
                                   , newUeCreated, ueDeleted
                                   )
import Equipment.Ue.Rest exposing (createUe, deleteUe, fetchStoredUes)

-- Main model.
type alias Model =
  { livePanel    : Equipment
  , errorMessage : Maybe String
  , enbModel     : EnbModel
  , mmeModel     : MmeModel
  , ueModel      : UeModel
  }

init : (Model, Cmd Msg)
init = ( { livePanel    = UE
         , errorMessage = Nothing
         , enbModel     = initEnb
         , mmeModel     = initMme
         , ueModel      = initUe
         }
       , Cmd.batch [ fetchStoredUes
                   , fetchStoredMmes
                   ]
       )

-- Main view.
view : Model -> Html Msg
view model =
  div [ A.class "w3-container"
      , A.style [("padding-top", "20px")]
      ]
    [ viewEquipmentSelectors model
    , viewErrorMessage model
    , viewEquipmentPanel model
    ]

viewEquipmentSelectors : Model -> Html Msg
viewEquipmentSelectors model =
  div [ A.class "w3-row-padding w3-margin-bottom" ]
      [ viewEquipmentSelector UE (numUes model.ueModel)
      , viewEquipmentSelector ENB (numEnbs model.enbModel)
      , viewEquipmentSelector MME (numMmes model.mmeModel)
      ]

viewEquipmentSelector : Equipment -> Int -> Html Msg
viewEquipmentSelector eq count =
  let (color, icon, label) = equipmentSelectorParams eq
      countStr             = toString count
  in
    div [ A.class "w3-third"
        , A.style [("cursor", "pointer")]
        , A.title <| "Switch to panel for " ++ label
        , E.onClick (SetLivePanel eq)
        ]
      [ div [ A.class ("w3-container w3-padding-16 " ++ color) ]
          [ div [ A.class "w3-left" ]
              [ i [ A.class "material-icons", A.style [("font-size", "36px")]]
                  [ text icon ]
              ]
          , div [ A.class "w3-right" ]
              [ h3 [ A.title <| "The number of " ++ label ]
                [ text countStr ]
              ]
          , div [ A.class "w3-clear" ] []
          , h4 [] [ text label ]
          ]
      ]

equipmentSelectorParams : Equipment -> (String, String, String)
equipmentSelectorParams eq =
  case eq of
    UE  -> ("w3-blue", "phone_iphone", "UEs")
    ENB -> ("w3-teal", "router", "ENBs")
    MME -> ("w3-blue-grey", "gamepad", "MMEs")

viewErrorMessage : Model -> Html Msg
viewErrorMessage model =
  case model.errorMessage of
    Just err ->
      div [ A.class "w3-container" ]
        [ div [ A.class "w3-panel w3-red" ]
            [ span [ A.class "w3-closebtn", E.onClick CloseErrorMsg ]
                [ Uni.text' "&#10005;" ]
            , h4 [] [ text "REST API Error" ]
            , p [] [ text err ]
            ]
        ]

    Nothing  -> div [] []

viewEquipmentPanel : Model -> Html Msg
viewEquipmentPanel model =
  case model.livePanel of
        UE  -> viewUePanel model.ueModel
        ENB -> viewEnbPanel model.enbModel
        MME -> viewMmePanel model.mmeModel

{-| Message pump for the app. Parts of the updating are delegated to
    other modules.
-}
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetLivePanel newEquipment ->
      ({model | livePanel = newEquipment}, Cmd.none)

    -- ENB stuff.
    OpenNewEnbForm            ->
      ({model | enbModel = openNewEnbForm model.enbModel}, Cmd.none)

    CancelNewEnbForm          ->
      ({model | enbModel = cancelNewEnbForm model.enbModel}, Cmd.none)

    OnInputNewEnb g name      ->
      ({model | enbModel = onInputNewEnb model.enbModel g name}, Cmd.none)

    SubmitNewEnbForm fields   ->
      ({model | enbModel = newEnbFormSubmitted model.enbModel}, createEnb fields)

    NewEnbCreated enb         ->
      ({model | enbModel = newEnbCreated model.enbModel enb}, Cmd.none)

    -- MME stuff.
    OpenNewMmeForm            ->
      ({model | mmeModel = openNewMmeForm model.mmeModel}, Cmd.none)

    CancelNewMmeForm          ->
      ({model | mmeModel = cancelNewMmeForm model.mmeModel}, Cmd.none)

    OnInputNewMmeName name    ->
      ({model | mmeModel = onInputNewMmeName model.mmeModel name}, Cmd.none)

    SubmitNewMmeForm name     ->
      ({model | mmeModel = newMmeFormSubmitted model.mmeModel}, createMme name)

    StoredMmesFetched mmes    ->
      ({model | mmeModel = storedMmesFetched model.mmeModel mmes}, Cmd.none)

    NewMmeCreated mme         ->
      ({model | mmeModel = newMmeCreated model.mmeModel mme}, Cmd.none)

    DeleteMme mme             ->
      (model, deleteMme mme)

    MmeDeleted mme            ->
      ({model | mmeModel = mmeDeleted model.mmeModel mme}, Cmd.none)

    -- UE stuff.
    OpenNewUeForm             ->
      ({model | ueModel = openNewUeForm model.ueModel}, Cmd.none)

    CancelNewUeForm           ->
      ({model | ueModel = cancelNewUeForm model.ueModel}, Cmd.none)

    OnInputNewUeImsi imsi     ->
      ({model | ueModel = onInputNewUeImsi model.ueModel imsi}, Cmd.none)

    SubmitNewUeForm imsi      ->
      ({model | ueModel = newUeFormSubmitted model.ueModel}, createUe imsi)

    StoredUesFetched ues      ->
      ({model | ueModel = storedUesFetched model.ueModel ues}, Cmd.none)

    NewUeCreated ue           ->
      ({model | ueModel = newUeCreated model.ueModel ue}, Cmd.none)

    DeleteUe ue               ->
      (model, deleteUe ue)

    UeDeleted ue              ->
      ({model | ueModel = ueDeleted model.ueModel ue}, Cmd.none)

    RestOpFailed error        ->
      ({model | errorMessage = Just <| expandError error}, Cmd.none)

    CloseErrorMsg             ->
      ({model | errorMessage = Nothing}, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

expandError : Error String -> String
expandError error =
  case error of
    Timeout                -> "Timeout"
    NetworkError           -> "Network Error"
    UnexpectedPayload err  -> "Unexpected Payload: " ++ err
    BadResponse resp       ->
      "Response (" ++ toString resp.status ++ "): " ++ resp.statusText
