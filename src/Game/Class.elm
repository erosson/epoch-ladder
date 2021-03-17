module Game.Class exposing (..)

import Dict exposing (Dict)
import Dict.Extra


type alias Class =
    { name : String
    , image : String
    }


sentinel =
    Class "Sentinel" "https://lastepoch.com/_nuxt/img/113c8d9.png"


mage =
    Class "Mage" "https://lastepoch.com/_nuxt/img/6bbd02d.png"


primalist =
    Class "Primalist" "https://lastepoch.com/_nuxt/img/21125b2.png"


acolyte =
    Class "Acolyte" "https://lastepoch.com/_nuxt/img/bcfad2c.png"


rogue =
    Class "Rogue" "https://lastepoch.com/_nuxt/img/33866b3.png"


list : List Class
list =
    [ sentinel, mage, primalist, acolyte, rogue ]


byName : Dict String Class
byName =
    Dict.Extra.fromListBy .name list


get : String -> Result String Class
get name =
    byName |> Dict.get name |> Result.fromMaybe name
