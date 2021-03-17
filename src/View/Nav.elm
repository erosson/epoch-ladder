module View.Nav exposing (view)

import Html as H exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events as E exposing (..)
import Route exposing (Route)
import Session exposing (LeaderboardRequest, Session)


view : LeaderboardRequest -> Html msg
view req =
    nav []
        [ ul []
            ([ "beta081", "beta080" ]
                |> List.map
                    (\v ->
                        if req.version == v then
                            [ text v ]

                        else
                            [ a [ Route.href <| Route.Leaderboard <| Session.toCode { req | version = v } ] [ text v ] ]
                    )
                |> List.map (li [])
            )
        , ul []
            ([ ( False, "softcore" ), ( True, "hardcore" ) ]
                |> List.map
                    (\( v, label ) ->
                        if req.hardcore == v then
                            [ text label ]

                        else
                            [ a [ Route.href <| Route.Leaderboard <| Session.toCode { req | hardcore = v } ] [ text label ] ]
                    )
                |> List.map (li [])
            )
        , ul []
            ([ ( False, "normal" ), ( True, "ssf" ) ]
                |> List.map
                    (\( v, label ) ->
                        if req.ssf == v then
                            [ text label ]

                        else
                            [ a [ Route.href <| Route.Leaderboard <| Session.toCode { req | ssf = v } ] [ text label ] ]
                    )
                |> List.map (li [])
            )
        , ul []
            (Session.classesList
                |> List.map (\v -> ( Just v, v ))
                |> (::) ( Nothing, "all classes" )
                |> List.map
                    (\( v, label ) ->
                        if req.class == v then
                            [ text label ]

                        else
                            [ a [ Route.href <| Route.Leaderboard <| Session.toCode { req | class = v } ] [ text label ] ]
                    )
                |> List.map (li [])
            )
        ]
