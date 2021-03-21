module Util exposing (..)

import FormatNumber
import FormatNumber.Locales exposing (usLocale)


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


ifthenfn : (x -> Bool) -> (x -> a) -> (x -> a) -> x -> a
ifthenfn pred t f val =
    if pred val then
        t val

    else
        f val


formatPercent : Float -> String
formatPercent f =
    (f * 100 |> round |> formatInt) ++ "%"


formatFloat : Float -> String
formatFloat =
    FormatNumber.format usLocale


formatInt : Int -> String
formatInt =
    toFloat
        >> FormatNumber.format { usLocale | decimals = FormatNumber.Locales.Exact 0 }
        >> ifthenfn ((==) "") (always "0") identity
