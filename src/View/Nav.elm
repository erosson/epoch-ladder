module View.Nav exposing (view)

import Html as H exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events as E exposing (..)
import Route exposing (HomeQuery, Route)
import Session exposing (LeaderboardRequest, Session)


view : HomeQuery -> Html msg
view q =
    nav []
        [ viewParamList q.version
            (\v -> { q | version = v })
            [ ( "beta081 (latest)", Nothing )
            , ( "beta080", Just "beta080" )
            ]
        , viewParamList q.hc
            (\v -> { q | hc = v })
            [ ( "softcore", False )
            , ( "hardcore", True )
            ]
        , viewParamList q.ssf
            (\v -> { q | ssf = v })
            [ ( "trade", False )
            , ( "ssf", True )
            ]
        , viewParamList q.class
            (\v -> { q | class = v })
            (Session.classesList
                |> List.map (\v -> ( v, Just v ))
                |> (::) ( "all classes", Nothing )
            )
        ]


viewParamList : v -> (v -> HomeQuery) -> List ( String, v ) -> Html msg
viewParamList val0 updater =
    let
        viewParam ( label, v ) =
            if val0 == v then
                [ text label ]

            else
                [ a [ updater v |> Route.Home |> Route.href ] [ text label ] ]
    in
    List.map viewParam >> List.map (li []) >> ul []
