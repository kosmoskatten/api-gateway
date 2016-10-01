module Equipment.Mme.Rest exposing
  ( fetchStoredMmes
  , createMme
  , deleteMme
  )

{-| REST API routines for the Mme. -}

import Array exposing (Array)
import HttpBuilder exposing (..)
import Maybe exposing (withDefault)
import Json.Decode as Dec
import Json.Encode as Enc
import String exposing (..)
import Task exposing (..)

import Types exposing (..)

{-| Fetch the already stored Mmes from the server. -}
fetchStoredMmes : Cmd Msg
fetchStoredMmes =
  Task.perform RestOpFailed StoredMmesFetched
      <| fetchStoredMmesTask `andThen` (\xs ->
            Task.sequence <| List.map resolveMmeTask xs
      )

{-| Command for creating a Mme, fetch its addresses and finally
    return a Mme record. -}
createMme : String -> Cmd Msg
createMme name =
  Task.perform RestOpFailed NewMmeCreated
    <| createMmeTask name `andThen` resolveMmeTask

{-| Command for deleting a Mme. -}
deleteMme : Mme -> Cmd Msg
deleteMme mme =
  Task.perform RestOpFailed MmeDeleted
    <| deleteMmeTask mme `andThen` (\_ -> succeed mme)

{-| Task for fetching all the stored Mmes. Returns a list of UrlRefs. -}
fetchStoredMmesTask : Task (HttpBuilder.Error String)
                           (List UrlRef)
fetchStoredMmesTask =
  (HttpBuilder.get "/api/v1/mme"
    |> withHeader "Accept" "application/json"
    |> HttpBuilder.send (jsonReader <| Dec.list urlRef) stringReader)
      `andThen` (\resp -> succeed resp.data)

{-| Resolve a Mme by fetching its IP configuration and the make the Mme. -}
resolveMmeTask : UrlRef -> Task (HttpBuilder.Error String) Mme
resolveMmeTask urlRef =
  fetchMmeIpConfigTask urlRef `andThen` (\addresses ->
      succeed { name      = withDefault "???" <| nameFromUrl urlRef
              , url       = urlRef.url
              , addresses = addresses
              }
    )

{-| Task that creates one Mme and returns the Mme's url ref. -}
createMmeTask : String -> Task (HttpBuilder.Error String) UrlRef
createMmeTask name =
  (HttpBuilder.post "/api/v1/mme"
    |> withJsonBody (Enc.object [("name", Enc.string name)])
    |> withHeaders [ ("Content-Type", "application/json")
                   , ("Accept", "application/json")]
    |> HttpBuilder.send (jsonReader urlRef) stringReader)
      `andThen` (\resp -> succeed resp.data)

{-| Task that take a Mme url ref and fetches all address for the Mme. -}
fetchMmeIpConfigTask : UrlRef -> Task (HttpBuilder.Error String) (Array String)
fetchMmeIpConfigTask urlRef =
  (HttpBuilder.get (urlRef.url ++ "/ip-config")
    |> withHeader "Accept" "application/json"
    |> HttpBuilder.send (jsonReader <| Dec.array Dec.string) stringReader)
      `andThen` (\resp -> succeed resp.data)

{-| Task that deletes one Mme. -}
deleteMmeTask : Mme -> Task (HttpBuilder.Error String)
                            (HttpBuilder.Response ())
deleteMmeTask mme =
  HttpBuilder.delete mme.url
    |> HttpBuilder.send unitReader stringReader

{-| Deconstruct the URL to get the Mme name. It's the 4th segment. -}
nameFromUrl : UrlRef -> Maybe String
nameFromUrl urlRef =
  List.head <| List.drop 4 <| split "/" urlRef.url
