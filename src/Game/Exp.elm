module Game.Exp exposing (..)

import Array exposing (Array)


{-| <https://lastepoch.gamepedia.com/Experience>

TODO: pretty sure these are wrong/too low!

-}
breakpoints : Array Int
breakpoints =
    Array.fromList
        -- 1 -> 2
        [ 0
        , 77
        , 238
        , 499
        , 879
        , 1396
        , 2070
        , 2921
        , 3972
        , 5245
        , 6765
        , 8554
        , 10640
        , 13048
        , 15807
        , 18944
        , 22489
        , 26473
        , 30928
        , 35886
        , 41382
        , 47449
        , 54127
        , 61452
        , 69466
        , 78210
        , 87729
        , 98070
        , 109284
        , 121424
        , 134550
        , 148723
        , 164014
        , 180500

        -- 35 -> 36
        , 198266
        , 217408
        , 238031
        , 260256
        , 284220
        , 310077
        , 338003
        , 368199
        , 400896
        , 436356
        , 474881
        , 516816
        , 562556
        , 612553
        , 667324
        , 727459
        , 793633
        , 866611
        , 947266
        , 1036590
        , 1135707
        , 1245891
        , 1368581
        , 1505401
        , 1658183
        , 1828987
        , 2020128
        , 2234197
        , 2474102
        , 2743088
        , 3044780
        , 3383215
        , 3762887
        , 4188787

        -- 69 -> 70
        , 4666454
        , 5202023
        , 5802283
        , 6474731
        , 7227643
        , 8070136
        , 9012246
        , 10065001
        , 11240510
        , 12552048
        , 14014157
        , 15642744
        , 17455195
        , 19470484
        , 21709306
        , 24194201
        , 26949699
        , 30002467
        , 33381467
        , 37118125
        , 41246509
        , 45803516
        , 50829072
        , 56366342
        , 62461959
        , 69166251
        , 76533499
        , 84622193
        , 93495314
        , 103220626
        , 113870983
        , 125524652

        -- duplicate the level 100 entry, so it always shows as full
        , 125524652
        ]


type alias Bounds =
    { lower : Int
    , upper : Int
    , diff : Int
    }


bounds : Int -> Maybe Bounds
bounds level =
    Maybe.map2
        (\lower upper ->
            { lower = lower
            , upper = upper
            , diff = upper - lower |> max 0
            }
        )
        (breakpoints |> Array.get (level - 1))
        (breakpoints |> Array.get level)


type alias Meter =
    { value : Int
    , percent : Float
    }


meter : { level : Int, exp : Int } -> Maybe ( Bounds, Maybe Meter )
meter { level, exp } =
    Maybe.map
        (\b ->
            let
                value =
                    -- ladder api tells us exp since last level, not total exp
                    -- exp - b.lower
                    exp

                meter_ =
                    if b.diff <= 0 then
                        { value = 1, percent = 1 } |> Just

                    else if value == clamp 0 b.diff value then
                        { value = value
                        , percent = toFloat value / toFloat b.diff |> clamp 0 1
                        }
                            |> Just

                    else
                        Nothing
            in
            ( b, meter_ )
        )
        (bounds level)
