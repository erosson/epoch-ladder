module Game.Subclass exposing (..)

import Dict exposing (Dict)
import Dict.Extra
import Game.Class as C exposing (Class)


type alias Subclass =
    { name : String
    , class : Class
    , image : String
    }



--[ ( "Sentinel", [ "Forge Guard", "Void Knight", "Paladin" ] )
--, ( "Mage", [ "Runemaster", "Spellblade", "Sorcerer" ] )
--, ( "Primalist", [ "Shaman", "Beastmaster", "Druid" ] )
--, ( "Acolyte", [ "Lich", "Necromancer", "Warlock" ] )
--, ( "Rogue", [ "Bladedancer", "Marksman", "Falconer" ] )
--]


list =
    -- https://www.lastepoch.com/classes
    [ Subclass "Paladin" C.sentinel "https://lastepoch.com/_nuxt/img/22a27ce.png"
    , Subclass "Void Knight" C.sentinel "https://lastepoch.com/_nuxt/img/678fe92.png"
    , Subclass "Forge Guard" C.sentinel "https://lastepoch.com/_nuxt/img/cc53f2a.png"
    , Subclass "Sorcerer" C.mage "https://lastepoch.com/_nuxt/img/31309bf.png"
    , Subclass "Spellblade" C.mage "https://lastepoch.com/_nuxt/img/e6f0b45.png"
    , Subclass "Runemaster" C.mage "https://lastepoch.com/_nuxt/img/bd6b197.png"
    , Subclass "Shaman" C.primalist "https://lastepoch.com/_nuxt/img/ae75138.png"
    , Subclass "Druid" C.primalist "https://lastepoch.com/_nuxt/img/84713d1.png"
    , Subclass "Beastmaster" C.primalist "https://lastepoch.com/_nuxt/img/8cb68b7.png"
    , Subclass "Necromancer" C.acolyte "https://lastepoch.com/_nuxt/img/cb2d93d.png"
    , Subclass "Lich" C.acolyte "https://lastepoch.com/_nuxt/img/a5c9996.png"
    , Subclass "Warlock" C.acolyte "https://lastepoch.com/_nuxt/img/263a946.png"
    , Subclass "Bladedancer" C.rogue "https://lastepoch.com/_nuxt/img/832a709.png"
    , Subclass "Marksman" C.rogue "https://lastepoch.com/_nuxt/img/d1989b3.png"
    , Subclass "Falconer" C.rogue "https://lastepoch.com/_nuxt/img/6027dc7.png"
    ]


unclassedList =
    -- it's possible to be on the leaderboard without choosing a subclass,
    -- in which case your subclass == your class
    list ++ (C.list |> List.map (\e -> Subclass e.name e e.image))


byName : Dict String Subclass
byName =
    Dict.Extra.fromListBy .name unclassedList


get : String -> Result String Subclass
get name =
    byName |> Dict.get name |> Result.fromMaybe name
