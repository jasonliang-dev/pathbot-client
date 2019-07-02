module Utils exposing
    ( curry
    , flip
    , getFirst
    , getSecond
    , map2Both
    , pointMagnitude
    , pointMap
    , pointMap2
    , uncurry
    )


getFirst : b -> List ( a, b ) -> Maybe a
getFirst b =
    List.filter (\( _, x ) -> x == b)
        >> List.head
        >> Maybe.map Tuple.first


getSecond : a -> List ( a, b ) -> Maybe b
getSecond a =
    List.filter (\( x, _ ) -> x == a)
        >> List.head
        >> Maybe.map Tuple.second


flip : (a -> b -> c) -> b -> a -> c
flip fn a b =
    fn b a


curry : (( a, b ) -> c) -> a -> b -> c
curry fn a b =
    fn ( a, b )


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry fn ( a, b ) =
    fn a b


pointMap : (a -> b) -> ( a, a ) -> ( b, b )
pointMap fn =
    Tuple.mapBoth fn fn


pointMap2 : (a -> b -> c) -> ( a, a ) -> ( b, b ) -> ( c, c )
pointMap2 fn =
    map2Both fn fn


map2Both : (a -> c -> x) -> (b -> d -> y) -> ( a, b ) -> ( c, d ) -> ( x, y )
map2Both fnFirst fnSecond ( a, b ) ( c, d ) =
    Tuple.mapBoth (fnFirst a) (fnSecond b) ( c, d )


pointMagnitude : ( Float, Float ) -> Float
pointMagnitude ( x, y ) =
    sqrt (x * x + y * y)
