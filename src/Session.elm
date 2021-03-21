module Session exposing
    ( Ability
    , Entry
    , Leaderboard
    , Msg(..)
    , Session
    , fetchLeaderboard
    , init
    , resetLocalFilters
    , toLeaderboard
    , toLeaderboardCode
    , toLeaderboardUrl
    , update
    )

import Browser.Navigation as Nav
import Dict exposing (Dict)
import Dict.Extra
import Game.Class exposing (Class)
import Game.Subclass exposing (Subclass)
import Http
import Json.Decode as D
import List.Extra
import RemoteData exposing (RemoteData)
import Route exposing (HomeQuery, homeQuery)
import Set exposing (Set)
import Util


type alias Session =
    { leaderboard : Dict String { req : Route.HomeQuery, res : RemoteData Http.Error Leaderboard }

    -- Nav.Key cannot be unit tested; Maybe Nav.Key is a workaround.
    -- See https://github.com/elm-explorations/test/issues/24
    , nav : Maybe Nav.Key
    }


type alias Leaderboard =
    { list : List Entry
    , rawList : List Entry
    , rankedList : List ( Int, Entry )
    , size : Int
    , rawSize : Int
    , subclasses : List ( Result String Subclass, Int )
    , classes : List ( Result String Class, Int )
    , abilityClasses : List ( ( Ability, Result String Subclass ), Int )
    , abilities : List ( Ability, Int )
    }


type alias Entry =
    { playerUsername : String
    , charName : String
    , charClass : Result String Subclass
    , charLvl : Int
    , exp : Int
    , deaths : Int
    , died : Bool
    , maxWave : Int
    , abilities : List Ability
    , abilitySet : Set String
    }


type alias Ability =
    { name : String

    -- for a few abilities, the api only returns the name
    , details : Maybe AbilityDetails
    }


type alias AbilityDetails =
    { imagePath : String
    , description : String
    , tags : String
    , cost : Int
    }


type Msg
    = HttpGetLeaderBoard HomeQuery (Result Http.Error (List Entry))


init : Maybe Nav.Key -> Session
init =
    Session Dict.empty


update : Msg -> Session -> Session
update msg session =
    case msg of
        HttpGetLeaderBoard filter res ->
            { session
                | leaderboard =
                    session.leaderboard
                        |> Dict.insert (toLeaderboardCode filter)
                            { req = filter
                            , res =
                                res
                                    |> Result.map (toLeaderboard filter)
                                    |> RemoteData.fromResult
                            }
            }


toLeaderboard : HomeQuery -> List Entry -> Leaderboard
toLeaderboard filter rawList =
    let
        popularity : (a -> comparable) -> List a -> List a -> List ( a, Int )
        popularity toKey all votes =
            let
                counts =
                    votes
                        |> List.Extra.gatherEquals
                        |> List.map (Tuple.mapSecond (List.length >> (+) 1))
                        |> List.sortBy Tuple.second
                        |> List.reverse

                counted : Set comparable
                counted =
                    counts |> List.map (Tuple.first >> toKey) |> Set.fromList

                uncounted =
                    all |> List.filter (\e -> Set.member (toKey e) counted |> not)
            in
            counts ++ List.map (\e -> ( e, 0 )) uncounted

        rankedList =
            rawList |> List.indexedMap Tuple.pair

        rankedFilteredList =
            rankedList
                |> List.filter
                    (Tuple.second
                        >> (\entry ->
                                List.all identity
                                    [ Set.isEmpty filter.subclass || Set.member (Util.unwrapResult identity .name entry.charClass) filter.subclass
                                    , Set.diff filter.skill entry.abilitySet |> Set.isEmpty
                                    ]
                           )
                    )

        filteredList =
            rankedFilteredList |> List.map Tuple.second

        abilities0 : List ( Ability, Int )
        abilities0 =
            filteredList
                |> List.concatMap .abilities
                |> List.filter (\a -> a.name /= "")
                |> popularity .name []

        abilitySet : Set String
        abilitySet =
            abilities0
                |> List.map (Tuple.first >> .name)
                |> Set.fromList

        -- old bug: filter for an ability + a class without that ability
        -- = can't unfilter the ability because, with no possible entries, it's
        -- not in the list. To fix that, we force all filtered abilities to
        -- appear in the ability list.
        abilities : List ( Ability, Int )
        abilities =
            abilities0
                ++ (Set.diff filter.skill abilitySet
                        |> Set.toList
                        |> List.map (\name -> ( Ability name Nothing, 0 ))
                   )
    in
    { list = filteredList
    , rankedList = rankedFilteredList
    , rawList = rawList
    , size = List.length filteredList
    , rawSize = List.length rawList
    , subclasses =
        rawList
            |> List.map .charClass
            |> popularity (Util.unwrapResult identity .name)
                (Game.Subclass.list
                    |> List.filter (\s -> filter.class == Nothing || filter.class == Just s.class.name)
                    |> List.map Ok
                )
    , classes =
        rawList
            |> List.map (.charClass >> Result.map .class)
            |> popularity (Util.unwrapResult identity .name)
                (Game.Class.list
                    |> List.filter (\c -> filter.class == Nothing || Just c.name == filter.class)
                    |> List.map Ok
                )
    , abilities = abilities
    , abilityClasses =
        filteredList
            |> List.concatMap (\e -> e.abilities |> List.map (\a -> ( a, e.charClass )))
            |> List.filter (\( a, _ ) -> a.name /= "")
            |> popularity (Tuple.first >> .name) []
    }


toLeaderboardCode : HomeQuery -> String
toLeaderboardCode req =
    [ req.version |> Maybe.withDefault "beta081"
    , Util.ifthen req.ssf "ssf" ""
    , Util.ifthen req.hc "hardcore" "softcore"
    , req.class |> Maybe.withDefault "allclass" |> String.toLower
    , req.rank |> Maybe.withDefault "arenawave"
    ]
        |> String.join ""


resetLocalFilters : HomeQuery -> HomeQuery
resetLocalFilters req =
    { homeQuery
        | version = req.version
        , ssf = req.ssf
        , hc = req.hc
        , class = req.class
        , rank = req.rank
    }


fetchLeaderboard : HomeQuery -> Session -> ( Session, Cmd Msg )
fetchLeaderboard req0 session =
    let
        req =
            -- TODO we should really have a narrower type here - but until then,
            -- remove any possible influence the client-side filters have
            req0 |> resetLocalFilters

        code =
            toLeaderboardCode req

        board =
            session.leaderboard
                |> Dict.get code
                |> Maybe.withDefault { req = req, res = RemoteData.NotAsked }
    in
    case board.res of
        -- set this leaderboard to loading, unless success or already-loading
        RemoteData.Loading ->
            ( session, Cmd.none )

        RemoteData.Success b ->
            ( session, Cmd.none )

        _ ->
            ( { session
                | leaderboard =
                    session.leaderboard
                        |> Dict.insert code { req = req, res = RemoteData.Loading }
              }
            , Http.get
                { url = toLeaderboardUrl req
                , expect = Http.expectJson (HttpGetLeaderBoard req) leaderboardResultDecoder
                }
            )


toLeaderboardUrl : HomeQuery -> String
toLeaderboardUrl q =
    "https://leapi.lastepoch.com/api/leader-board?code=" ++ toLeaderboardCode q


leaderboardResultDecoder : D.Decoder (List Entry)
leaderboardResultDecoder =
    D.andThen
        (\status ->
            case status of
                "success" ->
                    decodeData

                _ ->
                    D.fail <| "unknown status: " ++ status
        )
        (D.field "status" D.string)


decodeData : D.Decoder (List Entry)
decodeData =
    D.map8 Entry
        (D.field "player_username" D.string)
        (D.field "char_name" D.string)
        (D.field "char_class" <| D.map Game.Subclass.get D.string)
        (D.field "char_lvl" decodeIntString)
        (D.field "exp" decodeIntString)
        (D.field "deaths" D.int)
        (D.field "died" D.bool)
        (D.field "max_wave" decodeIntString)
        |> D.andThen
            (\entry ->
                D.map (\abilities -> entry abilities <| Set.fromList <| List.map .name abilities)
                    (D.map5 (\a b c d e -> [ a, b, c, d, e ])
                        (decodeAbility "1")
                        (decodeAbility "2")
                        (decodeAbility "3")
                        (decodeAbility "4")
                        (decodeAbility "5")
                    )
            )
        |> D.field "additionalData"
        |> D.list
        |> D.field "data"


decodeIntString : D.Decoder Int
decodeIntString =
    D.andThen
        (\s ->
            case String.toInt s of
                Nothing ->
                    D.fail <| "not an integer: " ++ s

                Just i ->
                    D.succeed i
        )
        D.string


decodeAbility : String -> D.Decoder Ability
decodeAbility n =
    D.map2 Ability
        (D.field ("abilityName_" ++ n) D.string)
        (D.maybe <|
            D.map4 AbilityDetails
                (D.field ("abilityImagePath_" ++ n) D.string)
                (D.field ("abilityDescription_" ++ n) D.string)
                (D.field ("abilityTags_" ++ n) D.string)
                (D.field ("abilityCost_" ++ n) decodeIntString)
        )
