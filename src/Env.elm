module Env exposing (..)

-- The Env.elm file is for per-environment configuration.
-- See https://dashboard.lamdera.app/docs/environment for more info.


parentPin : String
parentPin =
    "676767"

{-| Minutes offset from UTC (60 = CET winter in Spain, 120 = CEST summer)
-}
timezoneOffsetMinutes : Int
timezoneOffsetMinutes =
    60