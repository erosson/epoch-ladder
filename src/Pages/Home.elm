module Pages.Home exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Dict exposing (Dict)
import Html as H exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events as E exposing (..)
import List.Extra
import RemoteData exposing (RemoteData)
import Route exposing (Route)
import Session exposing (Ability, Entry, Session)
import View.Nav


type alias Model =
    { query : Route.HomeQuery
    , session : Session
    }


type Msg
    = SessionMsg Session.Msg


init : Route.HomeQuery -> Session -> ( Model, Cmd Msg )
init query session0 =
    let
        ( session, cmd ) =
            session0 |> Session.fetchLeaderboard query
    in
    ( Model query session
    , Cmd.map SessionMsg cmd
    )


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
    [ h1 []
        [ text "Last Epoch ladder popularity: "
        , text <| Session.toLeaderboardCode model.query
        ]
    , View.Nav.view model.query
    , case model.session.leaderboard of
        RemoteData.NotAsked ->
            text "loading."

        RemoteData.Loading ->
            text "loading..."

        RemoteData.Failure err ->
            code [] [ text <| Debug.toString err ]

        RemoteData.Success entries ->
            let
                subclassEntries : List ( String, Int )
                subclassEntries =
                    entries
                        |> List.map .charClass
                        |> List.Extra.gatherEquals
                        |> List.map (Tuple.mapSecond (List.length >> (+) 1))
                        |> List.sortBy Tuple.second
                        |> List.reverse

                classEntries : List ( String, Int )
                classEntries =
                    entries
                        |> List.filterMap (\e -> Dict.get e.charClass Session.subclasses)
                        |> List.Extra.gatherEquals
                        |> List.map (Tuple.mapSecond (List.length >> (+) 1))
                        |> List.sortBy Tuple.second
                        |> List.reverse

                abilityEntries : List ( ( Ability, String ), Int )
                abilityEntries =
                    entries
                        |> List.concatMap (\e -> e.abilities |> List.map (\a -> ( a, e.charClass )))
                        |> List.Extra.gatherEquals
                        |> List.map (Tuple.mapSecond (List.length >> (+) 1))
                        |> List.sortBy Tuple.second
                        |> List.reverse
            in
            div []
                [ details []
                    [ summary [] [ text "Ladder" ]
                    , table [] (entries |> List.indexedMap viewEntry |> List.map (tr []))
                    ]
                , details []
                    [ summary [] [ text "Popular subclasses" ]
                    , table []
                        (subclassEntries
                            |> List.indexedMap (viewSubclassEntry abilityEntries)
                            |> List.map (tr [])
                        )
                    ]
                , details []
                    [ summary [] [ text "Popular classes" ]
                    , table []
                        (classEntries
                            |> List.indexedMap viewClassEntry
                            |> List.map (tr [])
                        )
                    ]
                , details []
                    [ summary [] [ text "Popular abilities" ]
                    , table [ class "abilities" ]
                        (abilityEntries
                            |> List.indexedMap viewAbilityEntry
                            |> List.map (tr [])
                        )
                    ]
                ]
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
