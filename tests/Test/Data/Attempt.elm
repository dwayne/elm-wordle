module Test.Data.Attempt exposing (suite)

import Data.Attempt as Attempt exposing (Attempt)
import Data.Dictionary as Dictionary exposing (Dictionary)
import Data.Letter as Letter exposing (Letter)
import Data.Word as Word
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Data.Attempt" <|
        List.map
            testAttempt
            [ { target = "robot"
              , guess = "broom"
              , expected = [ a 'b', a 'r', a 'o', c 'o', i 'm' ]
              }
            , { target = "robot"
              , guess = "crown"
              , expected = [ i 'c', a 'r', a 'o', i 'w', i 'n' ]
              }
            , { target = "robot"
              , guess = "robot"
              , expected = [ c 'r', c 'o', c 'b', c 'o', c 't' ]
              }

            --
            -- The following test is based on
            -- https://github.com/Kinkelin/WordleCompetition/pull/8
            --
            , { target = "shine"
              , guess = "sense"
              , expected = [ c 's', i 'e', a 'n', i 's', c 'e' ]
              }
            ]


testAttempt : { target : String, guess : String, expected : Attempt } -> Test
testAttempt { target, guess, expected } =
    test (Debug.toString { target = target, guess = guess }) <|
        \_ ->
            let
                maybeTargetWord =
                    Word.fromString dictionary target

                maybeGuessWord =
                    Word.fromString dictionary guess

                maybeExpected =
                    Just expected

                maybeActual =
                    Maybe.map2 (Attempt.try Nothing) maybeTargetWord maybeGuessWord
            in
            Expect.equal maybeActual maybeExpected


dictionary : Dictionary
dictionary =
    Dictionary.fromList
        { wordLength = 5
        , answers =
            [ "robot"
            , "shine"
            ]
        , nonAnswers =
            [ "broom"
            , "crown"
            , "sense"
            ]
        }


c : Char -> Letter
c =
    Letter.Correct


a : Char -> Letter
a =
    Letter.AlmostCorrect


i : Char -> Letter
i =
    Letter.Incorrect
