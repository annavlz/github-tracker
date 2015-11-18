module GithubTracker where

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import String
import Task exposing (..)



-- VIEW

view : String -> String -> Html
view string imgUrl =
  div [ ]
    [ input
        [ placeholder "GitHub username"
        , Attr.value string
        , on "input" targetValue (Signal.message query.address)
        , class "input"
        ]
        []
    , img [src imgUrl
          , class "avatar"]
      [  ]
    ]


myStyle : List (String, String)
myStyle =
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]


-- WIRING

main : Signal Html
main =
  Signal.map2 view query.signal results.signal


results : Signal.Mailbox String
results =
  Signal.mailbox ""


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


getImage : String -> Task Http.Error String
getImage username =
        Http.get avatar ("https://api.github.com/users/" ++ username)
        `andThen`
            showPhoto


-- JSON DECODERS


type alias Avatar =
    { source : String
    }


avatar : Json.Decoder Avatar
avatar =
     Json.object1 Avatar
     ("avatar_url" := Json.string)


-- HANDLE RESPONSES


showPhoto : Avatar -> Task Http.Error String
showPhoto photo =
  succeed photo.source
