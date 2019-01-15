module Tests exposing
    ( aggregate
    , containingValues
    , cosWorksProperly
    , hull
    , intersection
    , intersectsAndIntersectionAreConsistent
    , sinWorksProperly
    )

import Angle exposing (Radians)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Interval
import Quantity exposing (Quantity, Unitless)
import Quantity.Interval exposing (Interval)
import Test exposing (Test)


fuzzer : Fuzzer (Interval Float Unitless)
fuzzer =
    let
        endpoint =
            Fuzz.map Quantity.float (Fuzz.floatRange -10 10)
    in
    Fuzz.map2 Quantity.Interval.from endpoint endpoint


floatEndpointsString : Interval.Interval Float -> String
floatEndpointsString interval =
    let
        ( minValue, maxValue ) =
            Interval.endpoints interval
    in
    String.concat
        [ "["
        , String.fromFloat minValue
        , ","
        , String.fromFloat maxValue
        , "]"
        ]


endpointsString : Interval Float Unitless -> String
endpointsString interval =
    let
        ( minValue, maxValue ) =
            Quantity.Interval.endpoints interval
    in
    String.concat
        [ "["
        , String.fromFloat (Quantity.toFloat minValue)
        , ","
        , String.fromFloat (Quantity.toFloat maxValue)
        , "]"
        ]


expectFloatIn : Interval.Interval Float -> Float -> Expectation
expectFloatIn interval value =
    let
        ( minValue, maxValue ) =
            Interval.endpoints interval

        tolerance =
            1.0e-12
    in
    if minValue - tolerance <= value && value <= maxValue + tolerance then
        Expect.pass

    else
        Expect.fail
            (String.fromFloat value
                ++ " is not contained in the interval "
                ++ floatEndpointsString interval
            )


expectQuantityIn : Interval Float Unitless -> Quantity Float Unitless -> Expectation
expectQuantityIn interval value =
    let
        ( minValue, maxValue ) =
            Quantity.Interval.endpoints interval

        tolerance =
            Quantity.float 1.0e-12
    in
    if
        ((minValue |> Quantity.minus tolerance)
            |> Quantity.lessThanOrEqualTo
                value
        )
            && (value
                    |> Quantity.lessThanOrEqualTo
                        (maxValue |> Quantity.plus tolerance)
               )
    then
        Expect.pass

    else
        Expect.fail
            (String.fromFloat (Quantity.toFloat value)
                ++ " is not contained in the interval "
                ++ endpointsString interval
            )


angleFuzzer : Fuzzer (Interval Float Radians)
angleFuzzer =
    let
        endpoint =
            Fuzz.map Angle.radians (Fuzz.floatRange (-2 * pi) (2 * pi))
    in
    Fuzz.map2 Quantity.Interval.from endpoint endpoint


sinWorksProperly : Test
sinWorksProperly =
    Test.fuzz2
        angleFuzzer
        (Fuzz.floatRange 0 1)
        "sin works as expected"
        (\interval t ->
            let
                valueInInterval =
                    Quantity.Interval.interpolate interval t
            in
            Angle.sin valueInInterval
                |> expectFloatIn
                    (Quantity.Interval.sin interval)
        )


cosWorksProperly : Test
cosWorksProperly =
    Test.fuzz2
        angleFuzzer
        (Fuzz.floatRange 0 1)
        "cos works as expected"
        (\interval t ->
            let
                valueInInterval =
                    Quantity.Interval.interpolate interval t
            in
            Angle.cos valueInInterval
                |> expectFloatIn
                    (Quantity.Interval.cos interval)
        )


containingValues : Test
containingValues =
    Test.fuzz (Fuzz.list (Fuzz.map Quantity.float Fuzz.float))
        "containing"
        (\values ->
            case Quantity.Interval.containingValues values of
                Just interval ->
                    interval
                        |> Expect.all
                            (List.map
                                (\value ->
                                    \theInterval ->
                                        value |> expectQuantityIn theInterval
                                )
                                values
                            )

                Nothing ->
                    values |> Expect.equal []
        )


hull : Test
hull =
    Test.describe "hull"
        [ Test.fuzz3
            fuzzer
            fuzzer
            (Fuzz.floatRange 0 1)
            "Value in first interval is in hull"
            (\firstInterval secondInterval t ->
                Quantity.Interval.interpolate firstInterval t
                    |> expectQuantityIn
                        (Quantity.Interval.hull firstInterval secondInterval)
            )
        , Test.fuzz3
            fuzzer
            fuzzer
            (Fuzz.floatRange 0 1)
            "Value in second interval is in hull"
            (\firstInterval secondInterval t ->
                Quantity.Interval.interpolate secondInterval t
                    |> expectQuantityIn
                        (Quantity.Interval.hull firstInterval secondInterval)
            )
        ]


expectDistinct : Interval Float Unitless -> Interval Float Unitless -> Expectation
expectDistinct firstInterval secondInterval =
    if
        (Quantity.Interval.minValue firstInterval
            |> Quantity.greaterThan
                (Quantity.Interval.maxValue secondInterval)
        )
            || (Quantity.Interval.maxValue firstInterval
                    |> Quantity.lessThan
                        (Quantity.Interval.minValue secondInterval)
               )
    then
        Expect.pass

    else
        Expect.fail <|
            "Intervals "
                ++ endpointsString firstInterval
                ++ " and "
                ++ endpointsString secondInterval
                ++ " are not distinct"


intersection : Test
intersection =
    Test.fuzz3
        fuzzer
        fuzzer
        (Fuzz.floatRange 0 1)
        "Value in intersection is in both intervals"
        (\firstInterval secondInterval t ->
            case Quantity.Interval.intersection firstInterval secondInterval of
                Just intersectionInterval ->
                    Quantity.Interval.interpolate intersectionInterval t
                        |> Expect.all
                            [ expectQuantityIn firstInterval
                            , expectQuantityIn secondInterval
                            ]

                Nothing ->
                    expectDistinct firstInterval secondInterval
        )


expectContainedIn : Interval Float Unitless -> Interval Float Unitless -> Expectation
expectContainedIn firstInterval secondInterval =
    if Quantity.Interval.isContainedIn firstInterval secondInterval then
        Expect.pass

    else
        Expect.fail <|
            "Interval "
                ++ endpointsString secondInterval
                ++ " is not contained in "
                ++ endpointsString firstInterval


aggregate : Test
aggregate =
    Test.fuzz (Fuzz.list fuzzer)
        "aggregate"
        (\intervals ->
            case Quantity.Interval.aggregate intervals of
                Just aggregateInterval ->
                    aggregateInterval
                        |> Expect.all
                            (List.map
                                (\interval ->
                                    \theAggregateInterval ->
                                        interval
                                            |> expectContainedIn
                                                theAggregateInterval
                                )
                                intervals
                            )

                Nothing ->
                    intervals |> Expect.equal []
        )


intersectsAndIntersectionAreConsistent : Test
intersectsAndIntersectionAreConsistent =
    Test.fuzz2
        fuzzer
        fuzzer
        "intersects and intersection are consistent"
        (\firstInterval secondInterval ->
            let
                intersects =
                    Quantity.Interval.intersects firstInterval secondInterval

                maybeIntersection =
                    Quantity.Interval.intersection firstInterval secondInterval
            in
            intersects |> Expect.equal (maybeIntersection /= Nothing)
        )
