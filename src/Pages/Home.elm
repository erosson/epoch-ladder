module Pages.Home exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Dict exposing (Dict)
import Html as H exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events as E exposing (..)
import List.Extra
import RemoteData exposing (RemoteData)
import Route exposing (Route)
import Session exposing (Ability, Entry, LeaderboardRequest, Session)
import View.Nav


type alias Model =
    { session : Session }


type Msg
    = SessionMsg Session.Msg


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session }, Cmd.none )


toSession : Model -> Session
toSession m =
    m.session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ session } as model) =
    case msg of
        SessionMsg submsg ->
            ( { model | session = session |> Session.update submsg }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> List (Html Msg)
view model =
    [ h1 [] [ text "Last Epoch ladder popularity" ]
    , View.Nav.view Session.defaultLeaderboardReq
    ]


viewEntry : Int -> Entry -> List (Html msg)
viewEntry index row =
    [ td [] [ text <| String.fromInt <| 1 + index ]
    , td [] [ text row.playerUsername ]
    , td [] [ text row.charName ]
    , td [] [ text row.charClass ]
    , td [] [ text <| String.fromInt row.charLvl ]
    , td [] [ text <| String.fromInt row.maxWave ]
    , td [ class "abilities" ] (row.abilities |> List.filterMap viewAbility)
    ]


viewClassEntry : Int -> ( String, Int ) -> List (Html msg)
viewClassEntry index ( class, count ) =
    [ td [] [ text <| String.fromInt <| 1 + index ]
    , td [] [ text class ]
    , td [] [ text <| String.fromInt count ]
    ]


viewSubclassEntry : List ( ( Ability, String ), Int ) -> Int -> ( String, Int ) -> List (Html msg)
viewSubclassEntry abilities index ( subclass, count ) =
    let
        class =
            Dict.get subclass Session.subclasses
    in
    [ td [] [ text <| String.fromInt <| 1 + index ]
    , td [] [ text subclass ]
    , td [] [ text <| Maybe.withDefault "???" class ]
    , td [] [ text <| String.fromInt count ]
    , td []
        [ details []
            [ summary [] [ text "abilities" ]
            , table [ A.class "abilities" ]
                (abilities
                    |> List.filter (\( ( a, s ), c ) -> s == subclass)
                    |> List.indexedMap viewAbilityEntry
                    |> List.map (tr [])
                )
            ]
        ]
    ]


viewAbilityEntry : Int -> ( ( Ability, String ), Int ) -> List (Html msg)
viewAbilityEntry index ( ( ability, subclass ), count ) =
    let
        class =
            Dict.get subclass Session.subclasses
    in
    [ td [] [ text <| String.fromInt <| 1 + index ]
    , td []
        (case ability.imagePath of
            Nothing ->
                []

            Just imagePath ->
                [ img [ src imagePath ] [] ]
        )
    , td []
        [ if ability.name == "" then
            i [] [ text "(empty)" ]

          else
            text ability.name
        ]
    , td [] [ text subclass ]
    , td [] [ text <| Maybe.withDefault "???" class ]
    , td [] [ text <| String.fromInt count ]
    ]


viewAbility : Ability -> Maybe (Html msg)
viewAbility a =
    a.imagePath
        |> Maybe.map
            (\imagePath ->
                img [ title a.name, src imagePath ] []
            )
