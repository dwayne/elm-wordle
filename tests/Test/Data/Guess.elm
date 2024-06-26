module Test.Data.Guess exposing (suite)

import Data.Answer as Answer
import Data.Dictionary as Dictionary exposing (Dictionary)
import Data.Guess as Guess exposing (Guess)
import Data.Letter as Letter exposing (Letter)
import Data.Word as Word
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Data.Guess" <|
        List.map
            testGuess
            [ { answer = "robot"
              , word = "broom"
              , expected = [ a 'b', a 'r', a 'o', c 'o', i 'm' ]
              }
            , { answer = "robot"
              , word = "crown"
              , expected = [ i 'c', a 'r', a 'o', i 'w', i 'n' ]
              }
            , { answer = "robot"
              , word = "robot"
              , expected = [ c 'r', c 'o', c 'b', c 'o', c 't' ]
              }

            --
            -- The following test is based on
            -- https://github.com/Kinkelin/WordleCompetition/pull/8
            --
            , { answer = "shine"
              , word = "sense"
              , expected = [ c 's', i 'e', a 'n', i 's', c 'e' ]
              }
            ]


testGuess : { answer : String, word : String, expected : Guess } -> Test
testGuess { answer, word, expected } =
    test (Debug.toString { answer = answer, word = word }) <|
        \_ ->
            let
                maybeAnswer =
                    Answer.fromString dictionary answer

                maybeGuess =
                    Word.fromString dictionary word

                maybeExpected =
                    Just expected

                maybeActual =
                    Maybe.map2 (Guess.guess Nothing) maybeAnswer maybeGuess
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
