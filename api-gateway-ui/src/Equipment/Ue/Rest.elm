module Equipment.Ue.Rest exposing
  ( fetchStoredUes
  , createUe
  , deleteUe
  )

{-| REST API routines for the UE. -}

import HttpBuilder exposing (..)
import Json.Decode as Dec
import Json.Encode as Enc
import Maybe exposing (withDefault)
import String exposing (split)
import Task exposing (..)

import Types exposing (..)

{-| Fetch already stored UEs from the server. -}
fetchStoredUes : Cmd Msg
fetchStoredUes =
  Task.perform RestOpFailed StoredUesFetched
    <| fetchStoredUesTask `andThen` (\xs ->
          Task.sequence <| List.map resolveUeTask xs
    )

{-| Command for creating a Ue, fetch its preferred cell and finally
    return an Ue record.
-}
createUe : String -> Cmd Msg
createUe imsi =
  Task.perform RestOpFailed NewUeCreated
    <| createUeTask imsi `andThen` resolveUeTask

{-| Command for deleting one Ue. -}
deleteUe : Ue -> Cmd Msg
deleteUe ue =
  Task.perform RestOpFailed UeDeleted
    <| deleteUeTask ue `andThen` (\_ -> succeed ue)

fetchStoredUesTask : Task (HttpBuilder.Error String) (List UrlRef)
fetchStoredUesTask =
  (HttpBuilder.get "/api/v1/msue"
    |> withHeader "Accept" "application/json"
    |> HttpBuilder.send (jsonReader <| Dec.list urlRef) stringReader)
      `andThen` (\resp -> succeed resp.data)

resolveUeTask : UrlRef -> Task (HttpBuilder.Error String) Ue
resolveUeTask urlRef =
  fetchPreferredEutranCellTask urlRef `andThen` (\pciRef ->
      succeed { imsi = withDefault "???" <| imsiFromUrl urlRef
              , url  = urlRef.url
              , pci  = pciRef.pci
              }
    )

fetchPreferredEutranCellTask : UrlRef -> Task (HttpBuilder.Error String) PciRef
fetchPreferredEutranCellTask urlRef =
  (HttpBuilder.get (urlRef.url ++ "/preferred-eutran-cell")
    |> withHeader "Accept" "application/json"
    |> HttpBuilder.send (jsonReader pciRef) stringReader)
      `andThen` (\resp -> succeed resp.data)

{-| Task that creates one Ue and return the Ue's url ref. -}
createUeTask : String -> Task (HttpBuilder.Error String) UrlRef
createUeTask imsi =
  (HttpBuilder.post "/api/v1/msue"
    |> withJsonBody (Enc.object [("imsi", Enc.string imsi)])
    |> withHeaders [ ("Content-Type", "application/json")
                   , ("Accept", "application/json") ]
    |> HttpBuilder.send (jsonReader urlRef) stringReader)
      `andThen` (\resp -> succeed resp.data)

{-| Task that deletes one Ue. -}
deleteUeTask : Ue -> Task (HttpBuilder.Error String) ()
deleteUeTask ue =
  (HttpBuilder.delete ue.url
    |> HttpBuilder.send unitReader stringReader)
      `andThen` (\_ -> succeed ())

{-| Deconstruct the URL to get the UE name. It's the 4th segment. -}
imsiFromUrl : UrlRef -> Maybe String
imsiFromUrl urlRef =
  List.head <| List.drop 4 <| split "/" urlRef.url
