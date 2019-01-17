module Quantity.Interval exposing
    ( Interval
    , singleton, fromEndpoints, from, containingValues, aggregate, hull, intersection
    , endpoints, minValue, maxValue, midpoint, width
    , contains, intersects, isContainedIn, isSingleton
    , interpolate
    , sin, cos
    )

{-|

@docs Interval


# Constructors

@docs singleton, fromEndpoints, from, containingValues, aggregate, hull, intersection


# Properties

@docs endpoints, minValue, maxValue, midpoint, width


# Queries

@docs contains, intersects, isContainedIn, isSingleton


# Interpolation

@docs interpolate


# Trigonometry

Note that in the same way that `Angle.sin` and `Angle.cos` from the `elm-units`
package take an `Angle` (a `Quantity Float Radians`) value and return a plain
`Float`, `sin` and `cos` in this package take an angle `Interval` from this
package (a `Quantity.Interval.Interval Float Radians`) and return a plain
`Interval Float` from the `elm-interval` package (an `Interval.Interval Float`).
As a result, the examples for `sin` and `cos` below use fully qualified module
names to avoid ambiguity.

@docs sin, cos

-}

import Angle exposing (Radians)
import Interval
import Quantity exposing (Quantity)


{-| Represents a finite, closed interval with a minimum and maximum value, for
example the interval from 0 degrees to 360 degrees. An `Interval number units`
represents a range of [`Quantity number units`][1] values.

[1]: https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/Quantity

-}
type Interval number units
    = Interval ( Quantity number units, Quantity number units )


{-| Construct a zero-width interval containing a single value.

    Interval.singleton (Length.meters 3)
    --> Interval.fromEndpoints
    -->     ( Length.meters 3
    -->     , Length.meters 3
    -->     )

-}
singleton : Quantity number units -> Interval number units
singleton value =
    Interval ( value, value )


{-| Construct an interval from its endpoints (the minimum and maximum values of
the interval).

    deliveryTime =
        Interval.fromEndpoints
            ( Duration.weeks 4
            , Duration.weeks 6
            )

The two values should be given in order but will be swapped if necessary to
ensure a valid interval is returned:

    Interval.endpoints
        ( Interval.fromEndpoints
            ( Duration.minutes 3
            , Duration.minutes 2
            )
        )
    --> ( Duration.minutes 2, Duration.minutes 3 )

-}
fromEndpoints : ( Quantity number units, Quantity number units ) -> Interval number units
fromEndpoints givenEndpoints =
    let
        ( firstValue, secondValue ) =
            givenEndpoints
    in
    if firstValue |> Quantity.lessThanOrEqualTo secondValue then
        Interval givenEndpoints

    else
        Interval ( secondValue, firstValue )


{-| Construct an interval with the two given endpoints (which can be provided in
either order).

    Interval.endpoints <|
        Interval.from
            (Angle.degrees 0)
            (Angle.degrees 180)
    --> ( Angle.degrees 0, Angle.degrees 180 )

    Interval.endpoints <|
        Interval.from
            (Angle.degrees 180)
            (Angle.degrees 0)
    --> ( Angle.degrees 0, Angle.degrees 180 )

-}
from : Quantity number units -> Quantity number units -> Interval number units
from firstValue secondValue =
    if firstValue |> Quantity.lessThanOrEqualTo secondValue then
        Interval ( firstValue, secondValue )

    else
        Interval ( secondValue, firstValue )


{-| Construct an interval containing all values in the given list. If the list
is empty, returns `Nothing`.

    Interval.containingValues
        [ Length.meters 2
        , Length.meters 1
        , Length.meters 3
        ]
    --> Just <|
    -->     Interval.from
    -->         (Length.meters 1)
    -->         (Length.meters 3)

    Interval.containingValues [ Length.meters -3 ]
    --> Just (Interval.singleton (Length.meters -3))

    Interval.containingValues []
    --> Nothing

-}
containingValues : List (Quantity number units) -> Maybe (Interval number units)
containingValues values =
    Maybe.map2 from (Quantity.minimum values) (Quantity.maximum values)


{-| Construct an interval containing both of the given intervals.

    firstInterval =
        Interval.from
            (Duration.hours 1)
            (Duration.hours 2)

    secondInterval =
        Interval.from
            (Duration.hours 3)
            (Duration.hours 6)

    Interval.hull firstInterval secondInterval
    --> Interval.from
    -->     (Duration.hours 1)
    -->     (Duration.hours 6)

-}
hull : Interval number units -> Interval number units -> Interval number units
hull firstInterval secondInterval =
    let
        ( min1, max1 ) =
            endpoints firstInterval

        ( min2, max2 ) =
            endpoints secondInterval
    in
    Interval ( Quantity.min min1 min2, Quantity.max max1 max2 )


{-| Attempt to construct an interval containing all the values common to both
given intervals. If the intervals do not intersect, returns `Nothing`.

    Interval.intersection
        (Interval.from (pixels 100) (pixels 300))
        (Interval.from (pixels 200) (pixels 500))
    --> Just (Interval.from (pixels 200) (pixels 300))

    Interval.intersection
        (Interval.from (pixels 100) (pixels 300))
        (Interval.from (pixels 400) (pixels 700))
    --> Nothing

If the two intervals just touch, a singleton interval will be returned:

    Interval.intersection
        (Interval.from (pixels 100) (pixels 300))
        (Interval.from (pixels 300) (pixels 500))
    --> Just (Interval.singleton (pixels 300))

-}
intersection : Interval number units -> Interval number units -> Maybe (Interval number units)
intersection firstInterval secondInterval =
    let
        ( min1, max1 ) =
            endpoints firstInterval

        ( min2, max2 ) =
            endpoints secondInterval

        maxOfMins =
            Quantity.max min1 min2

        minOfMaxes =
            Quantity.min max1 max2
    in
    if maxOfMins |> Quantity.lessThanOrEqualTo minOfMaxes then
        Just (Interval ( maxOfMins, minOfMaxes ))

    else
        Nothing


{-| Construct an interval containing all of the intervals in the given list. If
the list is empty, returns `Nothing`.

    Interval.aggregate
        [ Interval.singleton (Length.feet 2)
        , Interval.from
            (Length.feet 3)
            (Length.feet 4)
        ]
    --> Just (Interval.from (Length.feet 2) (Length.feet 4))

    Interval.aggregate []
    --> Nothing

-}
aggregate : List (Interval number units) -> Maybe (Interval number units)
aggregate intervals =
    case intervals of
        first :: rest ->
            Just (List.foldl hull first rest)

        [] ->
            Nothing


{-| Get the endpoints of an interval (its minimum and maximum values) as a
tuple. The first value will always be less than or equal to the second.

    ( minValue, maxValue ) =
        Interval.endpoints someInterval

For any interval,

    Interval.endpoints interval

is equivalent to (but more efficient than)

    ( Interval.minValue interval
    , Interval.maxValue interval
    )

-}
endpoints : Interval number units -> ( Quantity number units, Quantity number units )
endpoints (Interval intervalEndpoints) =
    intervalEndpoints


{-| Get the minimum value of an interval.

    Interval.minValue <|
        Interval.from
            (Length.meters 1)
            (Length.meters 3)
    --> Length.meters 1

-}
minValue : Interval number units -> Quantity number units
minValue interval =
    Tuple.first (endpoints interval)


{-| Get the maximum value of an interval.

    Interval.maxValue <|
        Interval.from
            (Length.meters 1)
            (Length.meters 3)
    --> Length.meters 3

-}
maxValue : Interval number units -> Quantity number units
maxValue interval =
    Tuple.second (endpoints interval)


{-| Get the midpoint of an interval.

    Interval.midpoint <|
        Interval.from
            (Duration.seconds 1)
            (Duration.seconds 4)
    --> Duration.seconds 2.5

-}
midpoint : Interval Float units -> Quantity Float units
midpoint interval =
    let
        ( intervalMinValue, intervalMaxValue ) =
            endpoints interval
    in
    Quantity.midpoint intervalMinValue intervalMaxValue


{-| Get the width of an interval.

    Interval.width <|
        Interval.from
            (Duration.seconds 1)
            (Duration.seconds 4)
    --> Duration.seconds 3

-}
width : Interval number units -> Quantity number units
width interval =
    let
        ( intervalMinValue, intervalMaxValue ) =
            endpoints interval
    in
    intervalMaxValue |> Quantity.minus intervalMinValue


{-| Interpolate between an interval's endpoints; a value of 0.0 corresponds to
the minimum value of the interval, a value of 0.5 corresponds to its midpoint
and a value of 1.0 corresponds to its maximum value. Values less than 0.0 or
greater than 1.0 can be used to extrapolate.

    oneTurn =
        Interval.from (Angle.degrees 0) (Angle.degrees 360)

    Interval.interpolate oneTurn 0
    --> Angle.degrees 0

    Interval.interpolate oneTurn 0.75
    --> Angle.degrees 270

    Interval.interpolate oneTurn -0.5
    --> Angle.degrees -180

Note that the interpolation is in fact from the minimum value to the maximum,
_not_ "from the first `Interval.from` argument to the second":

    Interval.interpolate
        (Interval.from
            (Angle.degrees 0)
            (Angle.degrees 180)
        )
        0.25
    --> Angle.degrees 45

    Interval.interpolate
        (Interval.from
            (Angle.degrees 180)
            (Angle.degrees 0)
        )
        0.25
    --> Angle.degrees 45 -- not 135!

-}
interpolate : Interval Float units -> Float -> Quantity Float units
interpolate interval t =
    let
        ( intervalMinValue, intervalMaxValue ) =
            endpoints interval
    in
    Quantity.interpolateFrom intervalMinValue intervalMaxValue t


{-| Check if an interval contains a given value.

    durationInterval =
        Interval.from
            (Duration.minutes 30)
            (Duration.minutes 90)

    durationInterval
        |> Interval.contains
            (Duration.hours 1)
    --> True


    durationInterval
        |> Interval.contains
            (Duration.hours 2)
    --> False

The minimum and maximum values of an interval are considered to be contained in
the interval:

    durationInterval
        |> Interval.contains
            (Duration.hours 0.5)
    --> True

-}
contains : Quantity number units -> Interval number units -> Bool
contains value interval =
    let
        ( intervalMinValue, intervalMaxValue ) =
            endpoints interval
    in
    (intervalMinValue |> Quantity.lessThanOrEqualTo value)
        && (value |> Quantity.lessThanOrEqualTo intervalMaxValue)


{-| Check if two intervals touch or overlap (have any values in common).

    distanceInterval =
        Interval.from
            (Length.kilometers 5)
            (Length.kilometers 10)

    distanceInterval
        |> Interval.intersects
            (Interval.from
                (Length.kilometers 8)
                (Length.kilometers 12)
            )
    --> True

    distanceInterval
        |> Interval.intersects
            (Interval.from
                (Length.kilometers 12)
                (Length.kilometers 15)
            )
    --> False

Intervals that just touch each other are considered to intersect (this is
consistent with `intersection` which will return a zero-width interval for the
intersection of two just-touching intervals):

    distanceInterval
        |> Interval.intersects
            (Interval.from
                (Length.kilometers 10)
                (Length.kilometers 15)
            )
    --> True

-}
intersects : Interval number units -> Interval number units -> Bool
intersects firstInterval secondInterval =
    let
        ( min1, max1 ) =
            endpoints firstInterval

        ( min2, max2 ) =
            endpoints secondInterval
    in
    (min1 |> Quantity.lessThanOrEqualTo max2)
        && (max1 |> Quantity.greaterThanOrEqualTo min2)


{-| Check if the second interval is fully contained in the first.

    angleInterval =
        Interval.from
            (Angle.degrees -30)
            (Angle.degrees 30)

    Interval.from (Angle.degrees -5) (Angle.degrees 15)
        |> Interval.isContainedIn angleInterval
    --> True

    Interval.from (Angle.degrees 15) (Angle.degrees 45)
        |> Interval.isContainedIn angleInterval
    --> False

Be careful with the argument order! If not using the `|>` operator, the first
example would be written as:

    Interval.isContainedIn angleInterval
        (Interval.from
            (Angle.degrees -5)
            (Angle.degrees 15)
        )
    --> True

-}
isContainedIn : Interval number units -> Interval number units -> Bool
isContainedIn firstInterval secondInterval =
    let
        ( min1, max1 ) =
            endpoints firstInterval

        ( min2, max2 ) =
            endpoints secondInterval
    in
    (min2 |> Quantity.greaterThanOrEqualTo min1)
        && (max2 |> Quantity.lessThanOrEqualTo max1)


{-| Check if the interval is a singleton (the minimum and maximum values are the
same).

    Interval.isSingleton
        (Interval.from (Length.meters 2) (Length.meters 2))
    --> True

    Interval.isSingleton
        (Interval.from (Length.meters 2) (Length.meters 3))
    --> False

-}
isSingleton : Interval number units -> Bool
isSingleton interval =
    let
        ( intervalMinValue, intervalMaxValue ) =
            endpoints interval
    in
    intervalMinValue == intervalMaxValue


shiftBy : Quantity number units -> Interval number units -> Interval number units
shiftBy delta interval =
    let
        ( intervalMinValue, intervalMaxValue ) =
            endpoints interval
    in
    Interval
        ( intervalMinValue |> Quantity.plus delta
        , intervalMaxValue |> Quantity.plus delta
        )


{-| For a given range of angle values, find the corresponding range of sine
values. Takes an `Interval` from this module and returns an `Interval` from the
`elm-interval` package.

    Quantity.Interval.sin <|
        Quantity.Interval.from
            (Angle.degrees 0)
            (Angle.degrees 45)
    --> Interval.from 0 0.7071

    Quantity.Interval.sin <|
        Quantity.Interval.from
            (Angle.degrees 0)
            (Angle.degrees 180)
    --> Interval.from 0 1

-}
sin : Interval Float Radians -> Interval.Interval Float
sin interval =
    if isSingleton interval then
        Interval.singleton (Angle.sin (minValue interval))

    else
        let
            ( includesMin, includesMax ) =
                sinIncludesMinMax interval

            ( intervalMinValue, intervalMaxValue ) =
                endpoints interval

            newMin =
                if includesMin then
                    -1

                else
                    min (Angle.sin intervalMinValue)
                        (Angle.sin intervalMaxValue)

            newMax =
                if includesMax then
                    1

                else
                    max (Angle.sin intervalMinValue)
                        (Angle.sin intervalMaxValue)
        in
        Interval.fromEndpoints ( newMin, newMax )


{-| For a given range of angle values, find the corresponding range of sine
values. Takes an `Interval` from this module and returns an `Interval` from the
`elm-interval` package.

    Quantity.Interval.cos <|
        Quantity.Interval.from
            (Angle.degrees 0)
            (Angle.degrees 45)
    --> Interval.from 0.7071 1

    Quantity.Interval.cos <|
        Quantity.Interval.from
            (Angle.degrees 0)
            (Angle.degrees 180)
    --> Interval.from -1 1

-}
cos : Interval Float Radians -> Interval.Interval Float
cos interval =
    if isSingleton interval then
        Interval.singleton (Angle.cos (minValue interval))

    else
        let
            ( includesMin, includesMax ) =
                cosIncludesMinMax interval

            ( intervalMinValue, intervalMaxValue ) =
                endpoints interval

            newMin =
                if includesMin then
                    -1

                else
                    min (Angle.cos intervalMinValue)
                        (Angle.cos intervalMaxValue)

            newMax =
                if includesMax then
                    1

                else
                    max (Angle.cos intervalMinValue)
                        (Angle.cos intervalMaxValue)
        in
        Interval.fromEndpoints ( newMin, newMax )


{-| cos(x - pi/2) = sin(x), therefore if cos(interval - pi/2) includes
the maximum/minimum, that means sin(interval) includes the maximum/minimum
accordingly.
-}
sinIncludesMinMax : Interval Float Radians -> ( Bool, Bool )
sinIncludesMinMax interval =
    interval |> shiftBy (Angle.radians (-pi / 2)) |> cosIncludesMinMax


{-| cos(x + pi) = -cos(x), therefore if cos(interval + pi) includes the maximum,
that means cos(interval) includes the minimum.
-}
cosIncludesMinMax : Interval Float Radians -> ( Bool, Bool )
cosIncludesMinMax interval =
    ( interval |> shiftBy (Angle.radians pi) |> cosIncludesMax
    , interval |> cosIncludesMax
    )


{-| The maximum of cos(x) is x = 2 pi \* k for every integer k.
If `minValue` and `maxValue` are in different branches
(meaning diffrent values of k), then the interval must pass through
2 pi \* k, which means the interval must include the maximum value.
-}
cosIncludesMax : Interval Float Radians -> Bool
cosIncludesMax interval =
    let
        ( intervalMinValue, intervalMaxValue ) =
            endpoints interval

        minBranch =
            floor <|
                Quantity.ratio intervalMinValue (Angle.radians (2 * pi))

        maxBranch =
            floor <|
                Quantity.ratio intervalMaxValue (Angle.radians (2 * pi))
    in
    minBranch /= maxBranch
