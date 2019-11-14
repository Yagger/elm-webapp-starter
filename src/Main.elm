port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Url
import Url.Parser as UrlParser exposing ((</>))
import Html exposing (..)
import Html.Attributes exposing (..)
import Debug
import Process
import Task

type alias Model =
  { key : Nav.Key
  , route : Route
  }

type Msg
  = OnUrlRequest Browser.UrlRequest
  | OnUrlChange Url.Url
  | OnReceiveMessageFromJS String
  | OnSendMessageToJS String

type Route = Home | Blog Int | NotFound

route : UrlParser.Parser (Route -> a) a
route =
  UrlParser.oneOf
    [ UrlParser.map Home UrlParser.top
    , UrlParser.map Blog (UrlParser.s "blog" </> UrlParser.int)
    ]

toRoute : Url.Url -> Route
toRoute url =
  Maybe.withDefault NotFound (UrlParser.parse route url)

init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init flags url key =
  ({key = key, route = toRoute url}, sendMessageToJS "ping")

view : Model -> Browser.Document Msg
view model =
  { title = "Elm application starter"
  , body =
    case model.route of
      Home ->
        [ ul []
          [ li [] [a [href "blog/1"] [text "Internal link 1"]]
          , li [] [a [href "blog/2"] [text "Internal link 2"]]
          , li [] [a [href "http://www.elm-lang.org", target "_blank"] [text "External link"]]
          ]
        ]
      Blog blogId ->
        [ div [] [text "Blog"]
        , ul []
          [ li [] [a [href "/"] [text "Home"]]
          ]
        ]
      NotFound ->
        [ div [] [text "Page not found"]
        , ul []
          [ li [] [a [href "/"] [text "Home"]]
          ]
        ]
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    OnUrlRequest urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          (model, Nav.replaceUrl model.key url.path)
        Browser.External href ->
          (model, Nav.load href)
    OnUrlChange url ->
      ({model | route = toRoute url}, Cmd.none)
    OnReceiveMessageFromJS s ->
      let
          a = Debug.log "<<<<" s
      in
      (model, Process.sleep 1000 |> Task.perform (always (OnSendMessageToJS "ping")))
    OnSendMessageToJS message->
      (model, sendMessageToJS message)

delay : Float -> msg -> Cmd msg
delay millis msg =
  Process.sleep millis
  |> Task.perform (\_ -> msg)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ receiveMessageFromJS OnReceiveMessageFromJS
    ]

main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlRequest = OnUrlRequest
    , onUrlChange = OnUrlChange
    }


port sendMessageToJS : String -> Cmd msg
port receiveMessageFromJS : (String -> msg) -> Sub msg