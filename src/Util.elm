module Util exposing (..)


unwrapResult : (err -> a) -> (ok -> a) -> Result err ok -> a
unwrapResult fromErr fromOk res =
    case res of
        Ok ok ->
            fromOk ok

        Err err ->
            fromErr err


ifthen : Bool -> a -> a -> a
ifthen pred t f =
    -- `elm-format`-friendly compact branching
    if pred then
        t

    else
        f
