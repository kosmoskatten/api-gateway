module Types exposing
  ( Equipment (..)
  , Msg (..)
  , Enb
  , Mme
  , Ue
  , PciRef
  , UrlRef
  , pciRef
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

  -- ENB stuff.
  | OpenNewEnbForm

  -- MME stuff.
  | OpenNewMmeForm
  | CancelNewMmeForm
  | OnInputNewMmeName String
  | SubmitNewMmeForm String
  | StoredMmesFetched (List Mme)
  | NewMmeCreated Mme
  | DeleteMme Mme
  | MmeDeleted Mme

  -- UE stuff.
  | OpenNewUeForm
  | CancelNewUeForm
  | OnInputNewUeImsi String
  | SubmitNewUeForm String
  | StoredUesFetched (List Ue)
  | NewUeCreated Ue
  | DeleteUe Ue
  | UeDeleted Ue

  -- General REST related stuff.
  | RestOpFailed (Error String)
  | CloseErrorMsg

type alias Enb =
  { name : String
  }

type alias Mme =
  { name      : String
  , url       : String
  , addresses : Array String
  }

type alias Ue =
  { imsi : String
  , url  : String
  , pci  : Maybe Int
  }

type alias PciRef =
  { pci : Maybe Int
  }

type alias UrlRef =
  { url : String
  }

{-| Json decoder for PciRef. -}
pciRef : Decoder PciRef
pciRef =
  object1 PciRef
    ("pci" := oneOf [null Nothing, map Just int])

{-| Json decoder for UrlRef. -}
urlRef : Decoder UrlRef
urlRef =
  object1 UrlRef
    ("url" := string)
