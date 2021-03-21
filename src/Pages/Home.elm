module Pages.Home exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Dict exposing (Dict)
import Game.Class exposing (Class)
import Game.Exp
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
    [ h1 [] [ a [ Route.href Route.home ] [ text "Epoch Rank" ] ]
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
                    , text <| Util.formatInt lb.size
                    , text " characters. "
                    , a [ Route.href <| Route.Home <| Session.resetLocalFilters model.query ] [ text "Reset filters" ]
                    ]
                , case ( model.query.rank, model.query.enableExp ) of
                    ( Just "level", True ) ->
                        p [] [ text "WARNING: data for the \"level\" leaderboard is questionable. This data's not visible on Last Epoch's website; it's probably not production-ready." ]

                    _ ->
                        p [] []
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
                            , tbody [] (lb.abilities |> List.map (viewAbilityFilter model.query lb))
                            ]
                        ]
                    , table []
                        [ thead []
                            [ tr [] <|
                                [ th [] []
                                , th [] []
                                , th [] [ text "Character" ]
                                , th [] [ text "Arena" ]
                                , th [] [ text "Skills" ]
                                , th [] [ text "Level" ]
                                ]
                                    ++ (if model.query.enableExp then
                                            [ th [] [ text "Exp" ], th [] [] ]

                                        else
                                            []
                                       )
                                    ++ [ th [] [ text "Deaths" ]
                                       ]
                            ]
                        , tbody [] (lb.rankedList |> List.map (viewEntry model.query))
                        ]
                    ]
                ]
    , ul []
        [ li []
            [ a [ target "_blank", href <| Session.toLeaderboardUrl model.query ]
                [ text "Last Epoch API data (", code [] [ text code_ ], text ")" ]
            ]
        , li []
            [ a [ target "_blank", href "https://github.com/erosson/epoch-rank" ]
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
            , div [] [ text <| Util.formatPercent <| toFloat count / toFloat lb.rawSize ]
            ]
        ]


viewAbilityFilter : Route.HomeQuery -> Leaderboard -> Result String ( Ability, Int ) -> Html msg
viewAbilityFilter query lb abil =
    -- old bug: filter for an ability + a class without that ability
    -- = can't unfilter the ability because, with no possible entries, it's
    -- not in the list. To fix that, we force all filtered abilities to
    -- appear in the ability list. Trouble is, we won't always have complete
    -- information for those missing abilities, hence the Result type - Err
    -- is just the ability name.
    let
        name =
            case abil of
                Err n ->
                    n

                Ok ( a, _ ) ->
                    a.name

        ( ability, count ) =
            abil
                |> Result.map (Tuple.mapFirst Just)
                |> Result.withDefault ( Nothing, 0 )

        selected =
            Set.member name query.skill

        querySkill =
            Util.ifthen selected (Set.remove name query.skill) (Set.insert name query.skill)

        td_ body =
            td [] [ a [ { query | skill = querySkill } |> Route.Home |> Route.href ] body ]

        percent =
            Util.formatPercent <| toFloat count / toFloat lb.size
    in
    tr
        [ classList
            [ ( "ability-filter-entry", True )
            , ( "selected", selected )
            ]
        ]
        -- [ td_ [ text <| Util.formatInt <| 1 + index ]
        [ td_
            [ ability
                |> Maybe.andThen .imagePath
                |> Maybe.map (\image -> img [ class "ability-icon", src image ] [])
                |> Maybe.withDefault (span [] [])
            ]
        , td_
            [ div []
                [ text name
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


viewEntry : Route.HomeQuery -> ( Int, Entry ) -> Html msg
viewEntry query ( index, row ) =
    tr
        [ classList
            [ ( "ladder-entry", True )
            , ( "dead", query.hc && row.died )
            ]
        ]
    <|
        [ td [ class "num" ] [ text <| Util.formatInt <| 1 + index, text ")" ]

        -- , td [] [ text row.playerUsername ]
        , td [] (viewClassIcon row.charClass)
        , td []
            [ div [] [ text row.charName ]
            , small [ class "username" ] [ text row.playerUsername ]
            ]

        -- , td [] (viewClass row.charClass)
        , td [ class "num" ] [ text <| Util.formatInt row.maxWave ]
        , td [] (row.abilities |> List.filterMap viewAbility)
        , td [ class "num" ] [ text <| Util.formatInt row.charLvl ]
        ]
            ++ (if query.enableExp then
                    let
                        expmeter =
                            Game.Exp.meter { level = row.charLvl, exp = row.exp }

                        exppct =
                            case expmeter of
                                Just ( bounds, Just exp ) ->
                                    exp.percent

                                _ ->
                                    1
                    in
                    [ td [ class "num" ]
                        [ div [] [ text <| Util.formatInt row.exp ]
                        , div [ class "exp-pct" ] [ text <| Util.formatPercent exppct ]
                        ]
                    , td []
                        (case expmeter of
                            Just ( bounds, exp ) ->
                                [ meter
                                    [ class "exp"
                                    , A.max <| String.fromInt <| Basics.max 1 bounds.diff
                                    , A.value <| String.fromInt <| Maybe.Extra.unwrap bounds.diff .value exp
                                    ]
                                    []
                                ]

                            Nothing ->
                                []
                        )
                    ]

                else
                    []
               )
            ++ [ td [ class "num" ] [ text <| Util.formatInt row.deaths ]
               ]


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
