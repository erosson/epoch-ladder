module Session exposing (Session, init)

import Browser.Navigation as Nav


type alias Session =
    -- Nav.Key cannot be unit tested; Maybe Nav.Key is a workaround.
    -- See https://github.com/elm-explorations/test/issues/24
    { nav : Maybe Nav.Key
    }


init : Maybe Nav.Key -> Session
init =
    Session
