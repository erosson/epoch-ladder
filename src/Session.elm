module Session exposing
    ( Ability
    , Entry
    , Leaderboard
    , Msg(..)
    , Session
    , fetchLeaderboard
    , init
    , toLeaderboard
    , toLeaderboardCode
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
    { leaderboard : RemoteData Http.Error Leaderboard

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
    , maxWave : Int
    , abilities : List Ability
    , abilitySet : Set String
    }


type alias Ability =
    { name : String, imagePath : Maybe String }


type Msg
    = HttpGetLeaderBoard HomeQuery (Result Http.Error (List Entry))


init : Maybe Nav.Key -> Session
init =
    Session RemoteData.NotAsked


update : Msg -> Session -> Session
update msg session =
    case msg of
        HttpGetLeaderBoard filter res ->
            { session | leaderboard = res |> Result.map (toLeaderboard filter) |> RemoteData.fromResult }


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
                                    [ filter.subclass == Nothing || filter.subclass == Just (Util.unwrapResult identity .name entry.charClass)
                                    , filter.skill |> Maybe.map (\skill -> Set.member skill entry.abilitySet) |> Maybe.withDefault True
                                    ]
                           )
                    )

        filteredList =
            rankedFilteredList |> List.map Tuple.second
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
    , abilities =
        filteredList
            |> List.concatMap .abilities
            |> List.filter (\a -> a.name /= "")
            |> popularity .name []
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
    , "arenawave"
    ]
        |> String.join ""


fetchLeaderboard : HomeQuery -> Session -> ( Session, Cmd Msg )
fetchLeaderboard req session =
    ( { session | leaderboard = RemoteData.Loading }
    , Http.get
        { url = "https://leapi.lastepoch.com/api/leader-board?code=" ++ toLeaderboardCode req
        , expect = Http.expectJson (HttpGetLeaderBoard { homeQuery | class = req.class }) leaderboardResultDecoder
        }
    )


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
    D.map5 Entry
        (D.field "player_username" D.string)
        (D.field "char_name" D.string)
        (D.field "char_class" <| D.map Game.Subclass.get D.string)
        (D.field "char_lvl" decodeIntString)
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
        (D.maybe <| D.field ("abilityImagePath_" ++ n) D.string)
