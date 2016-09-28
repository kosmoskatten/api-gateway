module Equipment.Ue.Rest exposing
  ( fetchStoredUes
  )

{-| REST API routines for the UE. -}

import HttpBuilder exposing (..)
import Json.Decode as Dec
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

imsiFromUrl : UrlRef -> Maybe String
imsiFromUrl urlRef =
  List.head <| List.drop 4 <| split "/" urlRef.url
