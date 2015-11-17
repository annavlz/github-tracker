module GithubTracker where

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import String
import Task exposing (..)
import Window


-- VIEW

view : Int -> String -> String -> Html
view h string imgUrl =
  div [ style (imgStyle h imgUrl) ]
    [ input
        [ placeholder "GitHub username"
        , Attr.value string
        , on "input" targetValue (Signal.message query.address)
        , style myStyle
        ]
        []
    ]


myStyle : List (String, String)
myStyle =
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]


imgStyle : Int -> String -> List (String, String)
imgStyle h src =
    [ ("background-image", "url('" ++ src ++ "')")
    , ("background-repeat", "no-repeat")
    , ("background-attachment", "fixed")
    , ("background-position", "center")
    , ("width", "100%")
    , ("height", toString h ++ "px")
    ]


-- WIRING

main : Signal Html
main =
  Signal.map3 view Window.height query.signal results.signal


results : Signal.Mailbox String
results =
  Signal.mailbox ""


port requestImg : Signal (Task Http.Error ())
port requestImg =
  query.signal
    |> sample getImage Window.dimensions
    |> Signal.map (\task -> task `andThen` Signal.send results.address)


sample f sampled events =
  Signal.sampleOn events (Signal.map2 f sampled events)


query : Signal.Mailbox String
query =
  Signal.mailbox ""


getImage : (Int,Int) -> String -> Task Http.Error String
getImage dimensions username =
        Http.get avatar ("https://api.github.com/users/" ++ username)
        `andThen`
            showPhoto dimensions


-- JSON DECODERS


type alias Avatar =
    { source : String
    }


avatar : Json.Decoder Avatar
avatar =
     Json.object1 Avatar
     ("avatar_url" := Json.string)


-- HANDLE RESPONSES


showPhoto : (Int,Int) -> Avatar -> Task Http.Error String
showPhoto (width,height) photo =
  succeed photo.source
