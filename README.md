# elm-units-interval

This package implements an `Interval` type similar to that from [`elm-interval`][elm-interval]
but based on the `Quantity` type from [`elm-units`][elm-units].

```elm
import Quantity.Interval as Interval exposing (Interval)
import Angle exposing (Radians)
import Pixels exposing (Pixels)

angleRange : Interval Float Radians
angleRange =
    Interval.from (Angle.degrees 0) (Angle.degrees 180)

widthRange : Interval Int Pixels
widthRange =
    Interval.from (Pixels.pixels 100) (Pixels.pixels 300)
```

Various functionality is included for constructing intervals (including as the
hull or intersection of other intervals) and checking for overlap, intersection
or containment:

```elm
import Quantity.Interval as Interval exposing (Interval)
import Length
import Duration

distanceInterval =
    Interval.from (Length.meters 10) (Length.meters 20)

Interval.endpoints distanceInterval
--> ( Length.meters 10, Length.meters 20 )

Interval.containingValues
    [ Length.feet 2
    , Length.feet 1
    , Length.feet 3
    ]
--> Just (Interval.from (Length.feet 1) (Length.feet 3))

Interval.hull
    (Interval.from
        (Duration.minutes 1)
        (Duration.minutes 2)
    )
    (Interval.from
        (Duration.minutes 3)
        (Duration.minutes 5)
    )
--> Interval.from (Duration.minutes 1) (Duration.minutes 5)

Interval.intersection
    (Interval.from
        (Duration.hours 1)
        (Duration.hours 3)
    )
    (Interval.from
        (Duration.hours 2)
        (Duration.hours 5)
    )
--> Just
-->     (Interval.from
-->         (Duration.hours 2)
-->         (Duration.hours 3)
-->     )

Interval.from (Angle.radians 0) (Angle.radians pi)
    |> Interval.contains (Angle.degrees 90)
--> True

Interval.from (Angle.radians 0) (Angle.radians pi)
    |> Interval.contains (Angle.degrees 270)
--> False
```

[elm-interval]: https://package.elm-lang.org/packages/ianmackenzie/elm-interval/latest/
[elm-units]: https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/
