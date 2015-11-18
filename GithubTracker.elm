module GithubTracker where

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import String
import Task exposing (..)



-- VIEW

view : String -> Data -> Html
view string data =
  div [ ]
    [ input
        [ placeholder "GitHub username"
        , Attr.value string
        , on "input" targetValue (Signal.message query.address)
        , class "input"
        ]
        [ ]
    , img [src data.source
          , class "avatar"]
        [ ]
    , p [ ]
        [ text ("Name: " ++ data.name) ]
    , p [ ]
        [ text ("Public repos: " ++ (toString data.repos))]
    ]


-- WIRING

main : Signal Html
main =
  Signal.map2 view query.signal results.signal


results : Signal.Mailbox Data
results =
  Signal.mailbox initialData


port requestImg : Signal (Task Http.Error ())
port requestImg =
  query.signal
    |> sample getImage
    |> Signal.map (\task -> task `andThen` Signal.send results.address)


sample f events =
  Signal.sampleOn events (Signal.map f events)


query : Signal.Mailbox String
query =
  Signal.mailbox ""


getImage : String -> Task Http.Error Data
getImage username =
        Http.get data ("https://api.github.com/users/" ++ username)
        `andThen`
            showData


-- JSON DECODERS


type alias Data =
    { source : String,
      repos : Int,
      name : String
    }


data : Json.Decoder Data
data =
     Json.object3 Data
     ("avatar_url" := Json.string)
     ("public_repos" := Json.int)
     ("name" := Json.string)


initialData : Data
initialData =
  { source = "",
    repos = 0,
    name = ""
  }
-- HANDLE RESPONSES


showData : Data -> Task Http.Error Data
showData data =
  succeed data
