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
            div []
                [ text <|
                    if Util.isTransientHttpError err then
                        "Error fetching leaderboard. Looks like a temporary error - try refreshing the page."

                    else
                        "Error fetching leaderboard. Looks like this is a bug in Epoch-Rank - please report this! https://github.com/erosson/epoch-rank/issues"
                , hr [] []
                , pre [] [ text <| Util.httpErrorToString err ]
                , hr [] []
                ]

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


viewAbilityFilter : Route.HomeQuery -> Leaderboard -> ( Ability, Int ) -> Html msg
viewAbilityFilter query lb ( ability, count ) =
    let
        selected =
            Set.member ability.name query.skill

        querySkill =
            Util.ifthen selected (Set.remove ability.name query.skill) (Set.insert ability.name query.skill)

        href : List (Html msg) -> Html msg
        href =
            a [ { query | skill = querySkill } |> Route.Home |> Route.href ]

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
        [ td []
            [ viewAbilityTooltip ability <|
                case ability.details of
                    Just a ->
                        [ href [ img [ class "ability-icon", src a.imagePath ] [] ] ]

                    Nothing ->
                        [ href [ span [ class "ability-icon" ] [] ] ]
            ]
        , td []
            [ href
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
        , td [] (row.abilities |> List.map viewAbilityIcon)
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


viewAbilityIcon : Ability -> Html msg
viewAbilityIcon ability =
    viewAbilityTooltip ability <|
        case ability.details of
            Just a ->
                [ img [ class "ability-icon", src a.imagePath ] [] ]

            Nothing ->
                [ span [ class "ability-icon" ] [] ]


viewAbilityTooltip : Ability -> List (Html msg) -> Html msg
viewAbilityTooltip ability body =
    case ability.details of
        Just a ->
            span [ class "tooltip" ] <|
                body
                    ++ [ span [ class "tooltip-body" ]
                            [ div [] [ b [] [ text ability.name ] ]
                            , p [ class "ability-cost" ]
                                [ text "Skill cost: "
                                , text <| Util.formatInt a.cost
                                ]
                            , p [] [ text a.description ]
                            , div [] [ text a.tags ]
                            ]
                       ]

        Nothing ->
            span [ class "tooltip" ] <|
                body
                    ++ [ span [ class "tooltip-body" ] [ div [] [ b [] [ text ability.name ] ] ]
                       ]
