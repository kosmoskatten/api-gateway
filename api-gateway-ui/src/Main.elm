import Html.App as App

import CsimControlApp exposing (init, view, update, subscriptions)

main : Program Never
main =
  App.program
    { init          = init
    , view          = view
    , update        = update
    , subscriptions = subscriptions
    }
