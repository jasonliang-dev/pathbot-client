module Utils exposing (flip, getFirst, getSecond)


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
