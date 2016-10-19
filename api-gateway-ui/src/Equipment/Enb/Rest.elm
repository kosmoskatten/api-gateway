module Equipment.Enb.Rest exposing
  ( createEnb
  )

{-| REST API routines for the ENB. -}

import Task exposing (..)

import Types exposing (..)

{-| Command for creating an ENB. -}
createEnb : NewEnbFormFields -> Cmd Msg
createEnb fields =
  Task.perform RestOpFailed NewEnbCreated
    <| succeed ({name = fields.newEnbName, url = ""})
