module Pages.Home exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Dict exposing (Dict)
import Html as H exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events as E exposing (..)
import Http
import Json.Decode as D
import List.Extra
import RemoteData exposing (RemoteData)
import Session exposing (Session)


type alias Model =
    { session : Session
    , value : Int
    , res : RemoteData Http.Error (List Entry)
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


subclassesList : List ( String, List String )
subclassesList =
    -- https://www.lastepoch.com/classes
    [ ( "Sentinel", [ "Forge Guard", "Void Knight", "Paladin" ] )
    , ( "Mage", [ "Runemaster", "Spellblade", "Sorcerer" ] )
    , ( "Primalist", [ "Shaman", "Beastmaster", "Druid" ] )
    , ( "Acolyte", [ "Lich", "Necromancer", "Warlock" ] )
    , ( "Rogue", [ "Bladedancer", "Marksman", "Falconer" ] )
    ]


subclasses : Dict String String
subclasses =
    subclassesList
        |> List.concatMap (\( cls, subs ) -> subs |> List.map (\sub -> ( sub, cls )))
        |> Dict.fromList


type Msg
    = Increment
    | Decrement
    | HttpRes (Result Http.Error (List Entry))


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , value = 0
      , res = RemoteData.Loading
      }
    , Http.get
        { url = "https://leapi.lastepoch.com/api/leader-board?code=beta081softcoreallclassarenawave"
        , expect = Http.expectJson HttpRes decoder
        }
    )


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


toSession : Model -> Session
toSession m =
    m.session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | value = model.value + 1 }, Cmd.none )

        Decrement ->
            ( { model | value = model.value - 1 }, Cmd.none )

        HttpRes res ->
            ( { model | res = res |> RemoteData.fromResult }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> List (Html Msg)
view model =
    [ h1 [] [ text "Last Epoch ladder popularity" ]
    , case model.res of
        RemoteData.NotAsked ->
            text "loading."

        RemoteData.Loading ->
            text "loading..."

        RemoteData.Failure err ->
            code [] [ text <| Debug.toString err ]

        RemoteData.Success entries ->
            let
                subclassEntries : List ( String, Int )
                subclassEntries =
                    entries
                        |> List.map .charClass
                        |> List.Extra.gatherEquals
                        |> List.map (Tuple.mapSecond (List.length >> (+) 1))
                        |> List.sortBy Tuple.second
                        |> List.reverse

                classEntries : List ( String, Int )
                classEntries =
                    entries
                        |> List.filterMap (\e -> Dict.get e.charClass subclasses)
                        |> List.Extra.gatherEquals
                        |> List.map (Tuple.mapSecond (List.length >> (+) 1))
                        |> List.sortBy Tuple.second
                        |> List.reverse

                abilityEntries : List ( ( Ability, String ), Int )
                abilityEntries =
                    entries
                        |> List.concatMap (\e -> e.abilities |> List.map (\a -> ( a, e.charClass )))
                        |> List.Extra.gatherEquals
                        |> List.map (Tuple.mapSecond (List.length >> (+) 1))
                        |> List.sortBy Tuple.second
                        |> List.reverse
            in
            div []
                [ details []
                    [ summary [] [ text "Ladder" ]
                    , table [] (entries |> List.indexedMap viewEntry |> List.map (tr []))
                    ]
                , details []
                    [ summary [] [ text "Popular subclasses" ]
                    , table []
                        (subclassEntries
                            |> List.indexedMap (viewSubclassEntry abilityEntries)
                            |> List.map (tr [])
                        )
                    ]
                , details []
                    [ summary [] [ text "Popular classes" ]
                    , table []
                        (classEntries
                            |> List.indexedMap viewClassEntry
                            |> List.map (tr [])
                        )
                    ]
                , details []
                    [ summary [] [ text "Popular abilities" ]
                    , table [ class "abilities" ]
                        (abilityEntries
                            |> List.indexedMap viewAbilityEntry
                            |> List.map (tr [])
                        )
                    ]
                ]
    ]


viewEntry : Int -> Entry -> List (Html msg)
viewEntry index row =
    [ td [] [ text <| String.fromInt <| 1 + index ]
    , td [] [ text row.playerUsername ]
    , td [] [ text row.charName ]
    , td [] [ text row.charClass ]
    , td [] [ text <| String.fromInt row.charLvl ]
    , td [] [ text <| String.fromInt row.maxWave ]
    , td [ class "abilities" ] (row.abilities |> List.filterMap viewAbility)
    ]


viewClassEntry : Int -> ( String, Int ) -> List (Html msg)
viewClassEntry index ( class, count ) =
    [ td [] [ text <| String.fromInt <| 1 + index ]
    , td [] [ text class ]
    , td [] [ text <| String.fromInt count ]
    ]


viewSubclassEntry : List ( ( Ability, String ), Int ) -> Int -> ( String, Int ) -> List (Html msg)
viewSubclassEntry abilities index ( subclass, count ) =
    let
        class =
            Dict.get subclass subclasses
    in
    [ td [] [ text <| String.fromInt <| 1 + index ]
    , td [] [ text subclass ]
    , td [] [ text <| Maybe.withDefault "???" class ]
    , td [] [ text <| String.fromInt count ]
    , td []
        [ details []
            [ summary [] [ text "abilities" ]
            , table [ A.class "abilities" ]
                (abilities
                    |> List.filter (\( ( a, s ), c ) -> s == subclass)
                    |> List.indexedMap viewAbilityEntry
                    |> List.map (tr [])
                )
            ]
        ]
    ]


viewAbilityEntry : Int -> ( ( Ability, String ), Int ) -> List (Html msg)
viewAbilityEntry index ( ( ability, subclass ), count ) =
    let
        class =
            Dict.get subclass subclasses
    in
    [ td [] [ text <| String.fromInt <| 1 + index ]
    , td []
        (case ability.imagePath of
            Nothing ->
                []

            Just imagePath ->
                [ img [ src imagePath ] [] ]
        )
    , td []
        [ if ability.name == "" then
            i [] [ text "(empty)" ]

          else
            text ability.name
        ]
    , td [] [ text subclass ]
    , td [] [ text <| Maybe.withDefault "???" class ]
    , td [] [ text <| String.fromInt count ]
    ]


viewAbility : Ability -> Maybe (Html msg)
viewAbility a =
    a.imagePath
        |> Maybe.map
            (\imagePath ->
                img [ title a.name, src imagePath ] []
            )
