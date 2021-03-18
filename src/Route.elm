module Route exposing
    ( HomeQuery
    , Route(..)
    , home
    , homeQuery
    , href
    , parse
    , pushUrl
    , pushUrl_
    , replaceUrl
    , replaceUrl_
    , toString
    )

import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html as H
import Html.Attributes as A
import Maybe.Extra
import Set exposing (Set)
import Url exposing (Url)
import Url.Builder as B
import Url.Parser as P exposing ((</>), (<?>), Parser)
import Url.Parser.Query as Q
import Util


type Route
    = Home HomeQuery
    | Debug


type alias HomeQuery =
    -- remote leaderboard query params
    { version : Maybe String
    , ssf : Bool
    , hc : Bool
    , class : Maybe String
    , rank : Maybe String

    -- local filters
    , subclass : Set String
    , skill : Set String

    -- feature switches. TODO: these should be in a separate record if we add more pages
    , enableExp : Bool
    }


homeQuery : HomeQuery
homeQuery =
    HomeQuery Nothing False False Nothing Nothing Set.empty Set.empty False


home : Route
home =
    Home homeQuery


parse : Url -> Maybe Route
parse =
    P.parse parser


parser : Parser (Route -> a) a
parser =
    P.oneOf
        [ P.map Home <|
            P.top
                <?> Q.map8 HomeQuery
                        (Q.string "version")
                        (boolQueryParser "ssf")
                        (boolQueryParser "hc")
                        (Q.string "class")
                        (Q.string "rank")
                        (Q.string "subclass" |> Q.map (Maybe.Extra.unwrap Set.empty (String.split "," >> Set.fromList)))
                        (Q.string "skill" |> Q.map (Maybe.Extra.unwrap Set.empty (String.split "," >> Set.fromList)))
                        (boolQueryParser "enableExp")
        , P.map Debug <| P.s "debug"
        ]


homeQueryBuilder : HomeQuery -> List B.QueryParameter
homeQueryBuilder q =
    [ q.version |> Maybe.map (B.string "version")
    , q.ssf |> boolQueryBuilder "ssf"
    , q.hc |> boolQueryBuilder "hc"
    , q.class |> Maybe.map (B.string "class")
    , q.rank |> Maybe.map (B.string "rank")
    , q.subclass
        |> Set.toList
        |> List.sort
        |> String.join ","
        |> Util.ifthenfn ((==) "") (always Nothing) Just
        |> Maybe.map (B.string "subclass")
    , q.skill
        |> Set.toList
        |> List.sort
        |> String.join ","
        |> Util.ifthenfn ((==) "") (always Nothing) Just
        |> Maybe.map (B.string "skill")
    , q.enableExp |> boolQueryBuilder "enableExp"
    ]
        |> List.filterMap identity


boolQueryParser : String -> Q.Parser Bool
boolQueryParser name =
    [ "true", "1" ]
        |> List.map (\s -> ( s, True ))
        |> Dict.fromList
        |> Q.enum name
        |> Q.map (Maybe.withDefault False)


boolQueryBuilder : String -> Bool -> Maybe B.QueryParameter
boolQueryBuilder name val =
    if val then
        B.string name "true" |> Just

    else
        Nothing


toString : Route -> String
toString route =
    case route of
        Home query ->
            B.absolute [] <| homeQueryBuilder query

        Debug ->
            B.absolute [ "debug" ] []


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
