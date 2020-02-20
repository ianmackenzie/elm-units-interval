module Temperature.Interval exposing
    ( Interval
    , singleton, fromEndpoints, from, containingValues, aggregate, hull, intersection
    , endpoints, minValue, maxValue, midpoint, width
    , contains, intersects, isContainedIn, isSingleton
    , interpolate
    )

{-| This module behaves just like `Quantity.Interval`, but works on
[`Temperature`](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/Temperature)
values.

@docs Interval


# Constructors

@docs singleton, fromEndpoints, from, containingValues, aggregate, hull, intersection


# Properties

@docs endpoints, minValue, maxValue, midpoint, width


# Queries

@docs contains, intersects, isContainedIn, isSingleton


# Interpolation

@docs interpolate

-}

import Angle exposing (Radians)
import Float.Extra as Float
import Interval
import Quantity exposing (Quantity)
import Temperature exposing (Temperature)


{-| Represents a finite, closed interval with a minimum and maximum temperature,
for example the interval from 20 to 30 degrees Celsius.
-}
type Interval
    = Interval ( Temperature, Temperature )


{-| Construct a zero-width interval containing a single temperature.
-}
singleton : Temperature -> Interval
singleton value =
    Interval ( value, value )


{-| Construct an interval from its endpoints (the minimum and maximum
temperatures of the interval). The two values should be given in order but will
be swapped if necessary to ensure a valid interval is returned.
-}
fromEndpoints : ( Temperature, Temperature ) -> Interval
fromEndpoints givenEndpoints =
    let
        ( firstValue, secondValue ) =
            givenEndpoints
    in
    if firstValue |> Temperature.lessThanOrEqualTo secondValue then
        Interval givenEndpoints

    else
        Interval ( secondValue, firstValue )


{-| Construct an interval with the two given endpoints (which can be provided in
either order).
-}
from : Temperature -> Temperature -> Interval
from firstValue secondValue =
    if firstValue |> Temperature.lessThanOrEqualTo secondValue then
        Interval ( firstValue, secondValue )

    else
        Interval ( secondValue, firstValue )


{-| Construct an interval containing all temperatures in the given list. If the
list is empty, returns `Nothing`.
-}
containingValues : List Temperature -> Maybe Interval
containingValues values =
    Maybe.map2 from (Temperature.minimum values) (Temperature.maximum values)


{-| Construct an interval containing both of the given intervals.
-}
hull : Interval -> Interval -> Interval
hull firstInterval secondInterval =
    let
        ( min1, max1 ) =
            endpoints firstInterval

        ( min2, max2 ) =
            endpoints secondInterval
    in
    Interval ( Temperature.min min1 min2, Temperature.max max1 max2 )


{-| Attempt to construct an interval containing all the temperatures common to
both given intervals. If the intervals do not intersect, returns `Nothing`. If
the two intervals just touch, a singleton interval will be returned.
-}
intersection : Interval -> Interval -> Maybe Interval
intersection firstInterval secondInterval =
    let
        ( min1, max1 ) =
            endpoints firstInterval

        ( min2, max2 ) =
            endpoints secondInterval

        maxOfMins =
            Temperature.max min1 min2

        minOfMaxes =
            Temperature.min max1 max2
    in
    if maxOfMins |> Temperature.lessThanOrEqualTo minOfMaxes then
        Just (Interval ( maxOfMins, minOfMaxes ))

    else
        Nothing


{-| Construct an interval containing all of the intervals in the given list. If
the list is empty, returns `Nothing`.
-}
aggregate : List Interval -> Maybe Interval
aggregate intervals =
    case intervals of
        first :: rest ->
            Just (List.foldl hull first rest)

        [] ->
            Nothing


{-| Get the endpoints of an interval (its minimum and maximum temperatures) as a
tuple. The first temperature will always be less than or equal to the second.
-}
endpoints : Interval -> ( Temperature, Temperature )
endpoints (Interval intervalEndpoints) =
    intervalEndpoints


{-| Get the minimum temperature of an interval.
-}
minValue : Interval -> Temperature
minValue interval =
    Tuple.first (endpoints interval)


{-| Get the maximum temperature of an interval.
-}
maxValue : Interval -> Temperature
maxValue interval =
    Tuple.second (endpoints interval)


{-| Get the midpoint of an interval.
-}
midpoint : Interval -> Temperature
midpoint interval =
    minValue interval |> Temperature.plus (Quantity.half (width interval))


{-| Get the width of an interval. This will never be negative. Note that this
returns a [`Temperature.Delta`](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/Temperature#Delta),
not a `Temperature`.
-}
width : Interval -> Temperature.Delta
width interval =
    let
        ( intervalMinValue, intervalMaxValue ) =
            endpoints interval
    in
    intervalMaxValue |> Temperature.minus intervalMinValue


{-| Interpolate between an interval's endpoints; a value of 0.0 corresponds to
the minimum temperature of the interval, a value of 0.5 corresponds to its
midpoint and a value of 1.0 corresponds to its maximum temperature. Values less
than 0.0 or greater than 1.0 can be used to extrapolate.
-}
interpolate : Interval -> Float -> Temperature
interpolate interval t =
    let
        ( intervalMinValue, intervalMaxValue ) =
            endpoints interval
    in
    Temperature.kelvins <|
        Float.interpolateFrom
            (Temperature.inKelvins intervalMinValue)
            (Temperature.inKelvins intervalMaxValue)
            t


{-| Check if an interval contains a given temperature. The minimum and maximum
temperatures of the interval are considered to be contained in the interval.
-}
contains : Temperature -> Interval -> Bool
contains value interval =
    let
        ( intervalMinValue, intervalMaxValue ) =
            endpoints interval
    in
    (intervalMinValue |> Temperature.lessThanOrEqualTo value)
        && (value |> Temperature.lessThanOrEqualTo intervalMaxValue)


{-| Check if two intervals touch or overlap (have any temperatures in common).
Intervals that just touch each other are considered to intersect.
-}
intersects : Interval -> Interval -> Bool
intersects firstInterval secondInterval =
    let
        ( min1, max1 ) =
            endpoints firstInterval

        ( min2, max2 ) =
            endpoints secondInterval
    in
    (min1 |> Temperature.lessThanOrEqualTo max2)
        && (max1 |> Temperature.greaterThanOrEqualTo min2)


{-| Check if the second interval is fully contained in the first.
-}
isContainedIn : Interval -> Interval -> Bool
isContainedIn firstInterval secondInterval =
    let
        ( min1, max1 ) =
            endpoints firstInterval

        ( min2, max2 ) =
            endpoints secondInterval
    in
    (min2 |> Temperature.greaterThanOrEqualTo min1)
        && (max2 |> Temperature.lessThanOrEqualTo max1)


{-| Check if the interval is a singleton (the minimum and maximum temperatures
are the same).
-}
isSingleton : Interval -> Bool
isSingleton interval =
    let
        ( intervalMinValue, intervalMaxValue ) =
            endpoints interval
    in
    intervalMinValue == intervalMaxValue
