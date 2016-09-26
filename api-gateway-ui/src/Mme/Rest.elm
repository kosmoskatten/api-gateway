module Mme.Rest exposing
  ( fetchStoredMmes
  , createMme
  , deleteMme
  )

{-| Rest API routines for the Mme. -}

import Array exposing (Array)
import HttpBuilder exposing (..)
import Json.Decode as Dec
import Json.Encode as Enc
import Task exposing (..)

import Types exposing (..)

{-| Fetch the already stored Mmes from the server. -}
fetchStoredMmes : Cmd Msg
fetchStoredMmes =
  Task.perform (\_ -> CloseErrorMsg) (StoredMmesFetched)
    <| succeed [ { name = "dummy"
                 , url = "dummier"
                 , addresses = Array.fromList ["1.2.3.4"]
                 }
               ]

{-| Command for creating a Mme, fetch its addresses and finally
    return a Mme record. -}
createMme : String -> Cmd Msg
createMme name =
  Task.perform RestOpFailed NewMmeCreated
    <| createMmeTask name `andThen` (\resp1 ->
         fetchMmeIpConfigTask resp1.data `andThen` (\resp2 ->
           succeed { name      = name
                   , url       = resp1.data.url
                   , addresses = resp2.data
                   }
         )
       )

{-| Command for deleting a Mme. -}
deleteMme : Mme -> Cmd Msg
deleteMme mme =
  Task.perform RestOpFailed MmeDeleted
    <| deleteMmeTask mme `andThen` (\_ -> succeed mme)

{-| Task that creates one Mme and returns the Mme's url ref. -}
createMmeTask : String -> Task (HttpBuilder.Error String)
                               (HttpBuilder.Response UrlRef)
createMmeTask name =
  HttpBuilder.post "/api/v1/mme"
    |> withJsonBody (Enc.object [("name", Enc.string name)])
    |> withHeaders [ ("Content-Type", "application/json")
                   , ("Accept", "application/json")]
    |> HttpBuilder.send (jsonReader urlRef) stringReader

{-| Task that take a Mme url ref and fetches all address for the Mme. -}
fetchMmeIpConfigTask : UrlRef -> Task (HttpBuilder.Error String)
                                      (HttpBuilder.Response (Array String))
fetchMmeIpConfigTask urlRef =
  HttpBuilder.get (urlRef.url ++ "/ip_config")
    |> withHeader "Accept" "application/json"
    |> HttpBuilder.send (jsonReader <| Dec.array Dec.string) stringReader

{-| Task that deletes one Mme. -}
deleteMmeTask : Mme -> Task (HttpBuilder.Error String)
                            (HttpBuilder.Response ())
deleteMmeTask mme =
  HttpBuilder.delete mme.url
    |> HttpBuilder.send unitReader stringReader
