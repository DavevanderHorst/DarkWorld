module Functions.Basic exposing (..)

import Models.MainModel exposing (Error)


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


addToTheBackOfTheList : a -> List a -> List a
addToTheBackOfTheList toBeAdded list =
    let
        reversedList =
            List.reverse list
    in
    List.reverse <| toBeAdded :: reversedList


returnTupleOfFirstThreeElementsOfList : List a -> Result Error ( a, a, a )
returnTupleOfFirstThreeElementsOfList list =
    let
        maybeFirst =
            List.head list

        maybeFirstTail =
            List.tail list
    in
    case ( maybeFirst, maybeFirstTail ) of
        ( Just first, Just firstTail ) ->
            let
                maybeSecond =
                    List.head firstTail

                maybeSecondTail =
                    List.tail firstTail
            in
            case ( maybeSecond, maybeSecondTail ) of
                ( Just second, Just secondTail ) ->
                    let
                        maybeThird =
                            List.head secondTail
                    in
                    case maybeThird of
                        Just third ->
                            Ok ( first, second, third )

                        Nothing ->
                            Err returnFirstThreeElementsError

                _ ->
                    Err returnFirstThreeElementsError

        _ ->
            Err returnFirstThreeElementsError


returnFirstThreeElementsError : Error
returnFirstThreeElementsError =
    { method = "Functions.Basic.returnTupleOfFirstThreeElementsOfList"
    , error = "List did not contain at least 3 elements"
    }
