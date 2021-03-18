module Pages.Home exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Dict exposing (Dict)
import Game.Class exposing (Class)
import Game.Subclass exposing (Subclass)
import Html as H exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events as E exposing (..)
import List.Extra
import Maybe.Extra
import RemoteData exposing (RemoteData)
import Route exposing (Route)
import Session exposing (Ability, Entry, Leaderboard, Session)
import Set exposing (Set)
import Util
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
    let
        code_ =
            Session.toLeaderboardCode model.query
    in
    [ h1 [] [ a [ Route.href Route.home ] [ text "Epoch Ladder" ] ]
    , p []
        [ text "An alternative view of "
        , a [ target "_blank", href "https://lastepoch.com/ladder" ] [ text "Last Epoch's ladder" ]
        , text " highlighting class and skill popularity."
        ]
    , View.Nav.view model.query
    , case model.session.leaderboard |> Dict.get code_ |> Maybe.Extra.unwrap RemoteData.NotAsked .res of
        RemoteData.NotAsked ->
            text "loading."

        RemoteData.Loading ->
            text "loading..."

        RemoteData.Failure err ->
            -- code [] [ text <| Debug.toString err ]
            code [] [ text "error fetching leaderboard" ]

        RemoteData.Success lb0 ->
            let
                lb =
                    lb0.rawList |> Session.toLeaderboard model.query
            in
            div []
                [ div [ class "ladder-classes" ] (lb.subclasses |> List.map (viewSubclassFilter model.query lb))
                , p []
                    [ text "Found "
                    , text <| String.fromInt lb.size
                    , text " characters. "
                    , a [ Route.href <| Route.Home <| Session.resetLocalFilters model.query ] [ text "Reset filters" ]
                    ]
                , div [ class "ladder-body" ]
                    [ div [ class "ability-filter" ]
                        [ table []
                            [ thead []
                                [ tr []
                                    [ th [] []
                                    , th [] [ text "Skill" ]
                                    , th [] []
                                    ]
                                ]
                            , tbody [] (lb.abilities |> List.indexedMap (viewAbilityFilter model.query lb))
                            ]
                        ]
                    , table []
                        [ thead []
                            [ tr []
                                [ th [] [ text "Character" ]
                                , th [] []
                                , th [] [ text "Level" ]
                                , th [] [ text "Arena" ]
                                , th [] [ text "Skills" ]
                                ]
                            ]
                        , tbody [] (lb.list |> List.indexedMap viewEntry |> List.map (tr [ class "ladder-entry" ]))
                        ]
                    ]
                ]
    , ul []
        [ li []
            [ a [ target "_blank", href <| Session.toLeaderboardUrl model.query ]
                [ text "Last Epoch API data (", code [] [ text code_ ], text ")" ]
            ]
        , li []
            [ a [ target "_blank", href "https://github.com/erosson/epoch-ladder" ]
                [ text "Source code" ]
            ]
        ]
    ]


viewSubclassFilter : Route.HomeQuery -> Leaderboard -> ( Result String Subclass, Int ) -> Html msg
viewSubclassFilter query lb ( subclass, count ) =
    let
        name : String
        name =
            subclass |> Util.unwrapResult identity .name

        selected =
            Set.member name query.subclass

        querySubclass =
            Util.ifthen selected (Set.remove name query.subclass) (Set.insert name query.subclass)
    in
    div
        [ classList
            [ ( "class-filter-entry", True )
            , ( "selected", selected )
            ]
        ]
        [ a [ { query | subclass = querySubclass } |> Route.Home |> Route.href ]
            [ div [] (viewClassIcon subclass)
            , small [] [ text name ]
            , div [] [ text <| formatPercent <| toFloat count / toFloat lb.rawSize ]
            ]
        ]


viewAbilityFilter : Route.HomeQuery -> Leaderboard -> Int -> ( Ability, Int ) -> Html msg
viewAbilityFilter query lb index ( ability, count ) =
    let
        selected =
            Set.member ability.name query.skill

        querySkill =
            Util.ifthen selected (Set.remove ability.name query.skill) (Set.insert ability.name query.skill)

        td_ body =
            td [] [ a [ { query | skill = querySkill } |> Route.Home |> Route.href ] body ]

        percent =
            formatPercent <| toFloat count / toFloat lb.size
    in
    tr
        [ classList
            [ ( "ability-filter-entry", True )
            , ( "selected", selected )
            ]
        ]
        -- [ td_ [ text <| String.fromInt <| 1 + index]
        [ td_
            [ ability.imagePath
                |> Maybe.map (\image -> img [ class "ability-icon", src image ] [])
                |> Maybe.withDefault (span [] [])
            ]
        , td_
            [ div []
                [ text ability.name
                , span [ class "percent" ] [ text percent ]
                ]
            , meter
                [ A.max <| String.fromInt lb.size
                , A.value <| String.fromInt count
                , title percent
                ]
                []
            ]
        ]


formatPercent : Float -> String
formatPercent f =
    (f * 100 |> round |> String.fromInt) ++ "%"


viewEntry : Int -> Entry -> List (Html msg)
viewEntry index row =
    -- [ td [] [ text <| String.fromInt <| 1 + index ]
    -- , td [] [ text row.playerUsername ]
    [ td []
        [ div [] [ text row.charName ]
        , small [ class "username" ] [ text row.playerUsername ]
        ]

    -- , td [] (viewClass row.charClass)
    , td [] (viewClassIcon row.charClass)
    , td [] [ text <| String.fromInt row.charLvl ]
    , td [] [ text <| String.fromInt row.maxWave ]
    , td [] (row.abilities |> List.filterMap viewAbility)
    ]


viewClassEntry : Int -> ( Result String Class, Int ) -> List (Html msg)
viewClassEntry index ( class, count ) =
    [ td [] [ 1 + index |> String.fromInt |> text ]
    , td [] (class |> viewClass)
    , td [] [ count |> String.fromInt |> text ]
    ]


viewSubclassEntry : List ( ( Ability, Result String Subclass ), Int ) -> Int -> ( Result String Subclass, Int ) -> List (Html msg)
viewSubclassEntry abilities index ( subclass, count ) =
    [ td [] [ 1 + index |> String.fromInt |> text ]
    , td [] (subclass |> viewClass)
    , td [] (subclass |> Result.mapError (always "???") |> Result.map .class |> viewClass)
    , td [] [ count |> String.fromInt |> text ]
    , td []
        [ details []
            [ summary [] [ text "abilities" ]
            , table []
                (abilities
                    |> List.filter (\( ( a, s ), c ) -> s == subclass)
                    |> List.indexedMap viewAbilityEntry
                    |> List.map (tr [])
                )
            ]
        ]
    ]


viewAbilityEntry : Int -> ( ( Ability, Result String Subclass ), Int ) -> List (Html msg)
viewAbilityEntry index ( ( ability, subclass ), count ) =
    [ td [] [ text <| String.fromInt <| 1 + index ]
    , td []
        (case ability.imagePath of
            Nothing ->
                []

            Just imagePath ->
                [ img [ class "ability-icon", src imagePath ] [] ]
        )
    , td []
        [ if ability.name == "" then
            i [] [ text "(empty)" ]

          else
            text ability.name
        ]
    , td [] (subclass |> viewClass)
    , td [] (subclass |> Result.mapError (always "???") |> Result.map .class |> viewClass)
    , td [] [ text <| String.fromInt count ]
    ]


viewClass : Result String { c | image : String, name : String } -> List (Html msg)
viewClass res =
    case res of
        Ok cls ->
            [ img [ class "class-icon", src cls.image ] [], text cls.name ]

        Err name ->
            [ text name ]


viewClassIcon : Result String { c | image : String, name : String } -> List (Html msg)
viewClassIcon res =
    case res of
        Ok cls ->
            [ img [ class "class-icon", src cls.image, title cls.name ] [] ]

        Err name ->
            [ text name ]


viewAbility : Ability -> Maybe (Html msg)
viewAbility a =
    a.imagePath
        |> Maybe.map
            (\imagePath ->
                img [ class "ability-icon", title a.name, src imagePath ] []
            )
