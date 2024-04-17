module BasicFunctions exposing (..)


isEvenIntNumber : Int -> Bool
isEvenIntNumber number =
    if modBy 2 number == 0 then
        True

    else
        False


differenceBetweenIntNumbers : Int -> Int -> Int
differenceBetweenIntNumbers num1 num2 =
    -- max number - min number
    let
        max =
            Basics.max num1 num2

        min =
            Basics.min num1 num2
    in
    max - min
