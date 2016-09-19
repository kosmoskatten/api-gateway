module Types exposing
  ( Equipment (..)
  , Msg (..)
  , Mme
  , UrlRef
  , urlRef
  )

{-| Generic types, shared between the modules -}

import Array exposing (Array)
import HttpBuilder exposing (Error)
import Json.Decode exposing (..)

type Equipment
  = UE
  | ENB
  | MME

type Msg
  = SetLivePanel Equipment
  | OpenNewMmeForm
  | CancelNewMmeForm
  | OnInputNewMmeName String
  | SubmitNewMmeForm String
  | NewMmeCreated Mme
  | DeleteMme Mme
  | MmeDeleted Mme
  | RestOpFailed (Error String)
  | CloseErrorMsg

type alias Mme =
  { name      : String
  , url       : String
  , addresses : Array String
  }

type alias UrlRef =
  { url : String
  }

{-| Json decoder for UrlRef -}
urlRef : Decoder UrlRef
urlRef =
  object1 UrlRef
    ("url" := string)
