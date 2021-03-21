module Util exposing (..)

import FormatNumber
import FormatNumber.Locales exposing (usLocale)
import Http


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


httpErrorToString : Http.Error -> String
httpErrorToString err =
    -- Debug.toString
    case err of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Network timeout"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus status ->
            "Bad status: " ++ String.fromInt status

        Http.BadBody decodeErr ->
            "Unexpected response: " ++ decodeErr


isTransientHttpError : Http.Error -> Bool
isTransientHttpError err =
    -- which errors are epoch-rank bugs, and which are intermittent?
    case err of
        Http.BadUrl _ ->
            False

        Http.BadBody _ ->
            False

        Http.BadStatus status ->
            status < 400 || status >= 500

        Http.Timeout ->
            True

        Http.NetworkError ->
            True
