module Pages.Home exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Html as H exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events as E exposing (..)
import Session exposing (Session)


type alias Model =
    { session : Session, value : Int }


type Msg
    = Increment
    | Decrement


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session, value = 0 }, Cmd.none )


toSession : Model -> Session
toSession m =
    m.session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | value = model.value + 1 }, Cmd.none )

        Decrement ->
            ( { model | value = model.value - 1 }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> List (Html Msg)
view model =
    [ img [ src "/logo.svg" ] []
    , h1 [] [ text "Your Elm App is working!" ]
    , div []
        [ button [ onClick Increment ] [ text "+" ]
        , text <| " " ++ String.fromInt model.value ++ " "
        , button [ onClick Decrement ] [ text "-" ]
        ]
    ]
