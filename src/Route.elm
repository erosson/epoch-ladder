module Route exposing (Route(..), href, parse, pushUrl, pushUrl_, replaceUrl, replaceUrl_, toString)

import Browser.Navigation as Nav
import Html as H
import Html.Attributes as A
import Url exposing (Url)
import Url.Parser as P exposing ((</>), Parser)


type Route
    = Home
    | Leaderboard String
    | Debug


parse : Url -> Maybe Route
parse =
    P.parse parser


parser : Parser (Route -> a) a
parser =
    P.oneOf
        [ P.map Home P.top
        , P.map Leaderboard <| P.s "leader-board" </> P.string
        , P.map Debug <| P.s "debug"
        ]


toString : Route -> String
toString route =
    case route of
        Home ->
            "/"

        Leaderboard code ->
            "/leader-board/" ++ code

        Debug ->
            "/debug"


href : Route -> H.Attribute msg
href =
    toString >> A.href


pushUrl_ : Nav.Key -> Route -> Cmd msg
pushUrl_ nav =
    toString >> Nav.pushUrl nav


pushUrl : Maybe Nav.Key -> Route -> Cmd msg
pushUrl mnav =
    case mnav of
        Nothing ->
            always Cmd.none

        Just nav ->
            pushUrl_ nav


replaceUrl_ : Nav.Key -> Route -> Cmd msg
replaceUrl_ nav =
    toString >> Nav.replaceUrl nav


replaceUrl : Maybe Nav.Key -> Route -> Cmd msg
replaceUrl mnav =
    case mnav of
        Nothing ->
            always Cmd.none

        Just nav ->
            replaceUrl_ nav
