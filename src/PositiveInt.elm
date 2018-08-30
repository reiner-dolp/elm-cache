module PositiveInt exposing (Error, PositiveInt, add, cast, errorToString, from, subs, toString, unwrap, zero)


type Error
    = FromNegative Int
    | SubstractionUnderflow Int Int


type PositiveInt
    = PositiveInt Int


errorToString : Error -> String
errorToString err =
    case err of
        FromNegative num ->
            "Cannot build a positive integer from the negative number " ++ String.fromInt num

        SubstractionUnderflow a b ->
            String.join "" [ String.fromInt a, " - ", String.fromInt b, " = ", String.fromInt (a - b), "is not a positive number" ]


from : Int -> Result Error PositiveInt
from value =
    if value < 0 then
        Err (FromNegative value)

    else
        Ok (PositiveInt value)


zero : PositiveInt
zero =
    PositiveInt 0


cast : Int -> PositiveInt
cast int =
    case from int of
        Err _ ->
            zero

        Ok positive ->
            positive


unwrap : PositiveInt -> Int
unwrap (PositiveInt int) =
    int


toString : PositiveInt -> String
toString (PositiveInt int) =
    String.fromInt int


add : PositiveInt -> PositiveInt -> PositiveInt
add (PositiveInt a) (PositiveInt b) =
    PositiveInt (a + b)


subs : PositiveInt -> PositiveInt -> Result Error PositiveInt
subs (PositiveInt a) (PositiveInt b) =
    if (a - b) < 0 then
        Err (SubstractionUnderflow a b)

    else
        Ok (PositiveInt (a - b))
