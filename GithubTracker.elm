module GithubTracker where

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import String
import Task exposing (..)
import Keyboard



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
    , img [src data.avatar
          , class "avatar"]
        [ ]
    , p [ ]
        [ text ("Name: " ++ data.name) ]
    , p [ ]
        [ text ("Public repos: " ++ (toString data.repos))]
    , p [ ]
        [ text ("Created at: " ++ data.createdAt) ]
    ]


-- WIRING

main : Signal Html
main =
  Signal.map2 view query.signal results.signal


results : Signal.Mailbox Data
results =
  Signal.mailbox initialData


port requestData : Signal (Task Http.Error ())
port requestData =
  query.signal
    |> sample getImage
    |> Signal.map (\task -> task
                   `andThen` Signal.send results.address)


sample get input =
  Signal.sampleOn Keyboard.enter (Signal.map get input)


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
    { avatar  : String,
      repos : Int,
      name : String,
      createdAt : String
    }


data : Json.Decoder Data
data =
     Json.object4 Data
     ("avatar_url" := Json.string)
     ("public_repos" := Json.int)
     ("name" := Json.string)
     ("created_at" := Json.string)


initialData : Data
initialData =
  { avatar  = "",
    repos = 0,
    name = "",
    createdAt = ""
  }
-- HANDLE RESPONSES


showData : Data -> Task Http.Error Data
showData data =
  succeed data
