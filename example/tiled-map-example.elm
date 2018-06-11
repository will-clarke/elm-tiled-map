module TiledMapExample exposing (..)

import Element exposing (Element)
import Html exposing (Html)
import Http
import TiledMap
import Tiled.Decode as Tiled


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = LevelLoaded (Result Http.Error Tiled.Level)


loadLevel : String -> Cmd Msg
loadLevel url =
    Http.send LevelLoaded <| Http.get url Tiled.decode


type alias Model =
    { map : Tiled.Level }


init : ( Model, Cmd Msg )
init =
    ( { map = Tiled.empty }
    , loadLevel "/example/assets/example-tilemap.json"
    )


subscriptions : Model -> Sub Msg
subscriptions mode =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LevelLoaded (Ok level) ->
            ( { model | map = level }, Cmd.none )

        LevelLoaded (Err _) ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div
        []
        [ TiledMap.gameMap model.map |> Element.toHtml
        ]
