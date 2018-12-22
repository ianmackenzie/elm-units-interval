# elm-quantity-interval

This package implements an `Interval` type similar to that from [`elm-interval`][elm-interval]
but based on the `Quantity` type from [`elm-units`][elm-units].

```elm
rgbRange : Interval Int
rgbRange =
    Interval.from 0 255

angleRange : Interval Float
angleRange =
    Interval.from 0 (2 * pi)
```

Various functionality is included for constructing intervals (including as the
hull or intersection of other intervals), checking for
overlap/intersection/containment, and performing limited arithmetic on
intervals:

```elm
unitInterval =
    Interval.from 0 1

Interval.endpoints unitInterval
--> ( 0, 1 )

Interval.containingValues [ 2, 1, 3 ]
--> Just (Interval.from 1 3)

Interval.hull
    (Interval.from 1 2)
    (Interval.from 3 5)
--> Interval.from 1 5

Interval.intersection
    (Interval.from 1 3)
    (Interval.from 2 5)
--> Just (Interval.from 2 3)

Interval.intersection
    (Interval.from 1 2)
    (Interval.from 3 5)
--> Nothing

Interval.contains 0 (Interval.from -1 3)
--> True

Interval.contains 5 (Interval.from -1 3)
--> False

Interval.sin (Interval.from 0 pi)
--> Interval.from 0 1
```

[elm-interval]: https://package.elm-lang.org/packages/ianmackenzie/elm-interval/latest/
[elm-units]: https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/
