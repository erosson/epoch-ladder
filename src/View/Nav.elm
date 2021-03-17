module View.Nav exposing (view)

import Dict exposing (Dict)
import Game.Class exposing (Class)
import Html as H exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events as E exposing (..)
import Route exposing (HomeQuery, Route)
import Session exposing (Session)


view : HomeQuery -> Html msg
view q =
    nav []
        [ viewParamList q.version
            (\v -> { q | version = v })
            [ ( [ text "beta081 (latest)" ], Nothing )
            , ( [ text "beta080" ], Just "beta080" )
            ]
        , viewParamList q.hc
            (\v -> { q | hc = v })
            [ ( [ text "softcore" ], False )
            , ( [ text "hardcore" ], True )
            ]
        , viewParamList q.ssf
            (\v -> { q | ssf = v })
            [ ( [ text "trade" ], False )
            , ( [ text "ssf" ], True )
            ]
        , viewParamList (q.class |> Maybe.andThen (Game.Class.get >> Result.toMaybe))
            (\v -> { q | class = v |> Maybe.map .name })
            (Game.Class.list
                |> List.map (\v -> ( [ img [ class "class-icon", src v.image ] [], text v.name ], Just v ))
                |> (::) ( [ text "all classes" ], Nothing )
            )
        ]


viewParamList : v -> (v -> HomeQuery) -> List ( List (Html msg), v ) -> Html msg
viewParamList val0 updater =
    let
        viewParam ( label, v ) =
            if val0 == v then
                label

            else
                [ a [ updater v |> Route.Home |> Route.href ] label ]
    in
    List.map viewParam >> List.map (li []) >> ul []
