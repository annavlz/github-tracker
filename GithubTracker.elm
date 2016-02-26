module GithubTracker where

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import String
import Task exposing (..)
import Keyboard

--MODEL

type alias Model =
    { avatar  : String,
      repos : Int,
      name : String,
      createdAt : String
    }


initialModel : Model
initialModel =
  { avatar  = "",
    repos = 0,
    name = "",
    createdAt = ""
  }


--UPDATE

port requestData : Signal (Task Http.Error ())
port requestData =
  query.signal
    |> sample getImage
    |> Signal.map (\task -> task
                   `andThen` Signal.send results.address)


sample: (String -> Task Http.Error Model)
      -> Signal String
      -> Signal (Task Http.Error Model)
sample getImage input =
  Signal.sampleOn Keyboard.enter (Signal.map getImage input)


getImage : String -> Task Http.Error Model
getImage username =
  Http.get data ("https://api.github.com/users/" ++ username)


data : Json.Decoder Model
data =
     Json.object4 Model
     ("avatar_url" := Json.string)
     ("public_repos" := Json.int)
     ("name" := Json.string)
     ("created_at" := Json.string)


-- VIEW

view : String -> Model -> Html
view username model =
  div [ ]
    [ input
        [ placeholder "GitHub username"
        , Attr.value username

        , on "input" targetValue (Signal.message query.address)
        , class "input"
        ]
        [ ]
    , img [src model.avatar
          , class "avatar"]
        [ ]
    , p [ ]
        [ text ("Name: " ++ model.name) ]
    , p [ ]
        [ text ("Public repos: " ++ (toString model.repos))]
    , p [ ]
        [ text ("Created at: " ++ model.createdAt) ]
    ]


--MAILBOXES

query : Signal.Mailbox String
query =
  Signal.mailbox ""

results : Signal.Mailbox Model
results =
  Signal.mailbox initialModel


-- WIRING

main : Signal Html
main =
  Signal.map2 view query.signal results.signal

