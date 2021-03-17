module Session exposing
    ( Ability
    , Entry
    , LeaderboardRequest
    , Msg(..)
    , Session
    , classesList
    , defaultLeaderboardReq
    , fetchLeaderboard
    , init
    , subclasses
    , toLeaderboardCode
    , update
    )

import Browser.Navigation as Nav
import Dict exposing (Dict)
import Http
import Json.Decode as D
import RemoteData exposing (RemoteData)


type alias Session =
    -- Nav.Key cannot be unit tested; Maybe Nav.Key is a workaround.
    -- See https://github.com/elm-explorations/test/issues/24
    { leaderboard : RemoteData Http.Error (List Entry)
    , nav : Maybe Nav.Key
    }


type alias Entry =
    { playerUsername : String
    , charName : String
    , charClass : String
    , charLvl : Int
    , maxWave : Int
    , abilities : List Ability
    }


type alias Ability =
    { name : String, imagePath : Maybe String }


type alias LeaderboardRequest r =
    { r
        | version : Maybe String
        , ssf : Bool
        , hc : Bool
        , class : Maybe String
    }


defaultLeaderboardReq =
    { version = Nothing, ssf = False, hc = False, class = Nothing }


type Msg
    = HttpGetLeaderBoard (Result Http.Error (List Entry))


init : Maybe Nav.Key -> Session
init =
    Session RemoteData.NotAsked


update : Msg -> Session -> Session
update msg session =
    case msg of
        HttpGetLeaderBoard res ->
            { session | leaderboard = res |> RemoteData.fromResult }


toLeaderboardCode : LeaderboardRequest r -> String
toLeaderboardCode req =
    [ req.version |> Maybe.withDefault "beta081"
    , ifthen req.ssf "ssf" ""
    , ifthen req.hc "hardcore" "softcore"
    , req.class |> Maybe.withDefault "allclass" |> String.toLower
    , "arenawave"
    ]
        |> String.join ""


ifthen : Bool -> a -> a -> a
ifthen pred t f =
    -- `elm-format`-friendly compact branching
    if pred then
        t

    else
        f


fetchLeaderboard : LeaderboardRequest r -> Session -> ( Session, Cmd Msg )
fetchLeaderboard req session =
    ( { session | leaderboard = RemoteData.Loading }
    , Http.get
        { url = "https://leapi.lastepoch.com/api/leader-board?code=" ++ toLeaderboardCode req
        , expect = Http.expectJson HttpGetLeaderBoard decoder
        }
    )


subclassesList : List ( String, List String )
subclassesList =
    -- https://www.lastepoch.com/classes
    [ ( "Sentinel", [ "Forge Guard", "Void Knight", "Paladin" ] )
    , ( "Mage", [ "Runemaster", "Spellblade", "Sorcerer" ] )
    , ( "Primalist", [ "Shaman", "Beastmaster", "Druid" ] )
    , ( "Acolyte", [ "Lich", "Necromancer", "Warlock" ] )
    , ( "Rogue", [ "Bladedancer", "Marksman", "Falconer" ] )
    ]


classesList : List String
classesList =
    subclassesList |> List.map Tuple.first


subclasses : Dict String String
subclasses =
    subclassesList
        |> List.concatMap (\( cls, subs ) -> ( cls, cls ) :: List.map (\sub -> ( sub, cls )) subs)
        |> Dict.fromList


decoder : D.Decoder (List Entry)
decoder =
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
    D.map6 Entry
        (D.field "player_username" D.string)
        (D.field "char_name" D.string)
        (D.field "char_class" D.string)
        (D.field "char_lvl" decodeIntString)
        (D.field "max_wave" decodeIntString)
        (D.map5 (\a b c d e -> [ a, b, c, d, e ])
            (decodeAbility "1")
            (decodeAbility "2")
            (decodeAbility "3")
            (decodeAbility "4")
            (decodeAbility "5")
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
