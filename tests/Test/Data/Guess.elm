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
              , guess = "broom"
              , expected = [ p 'b', p 'r', p 'o', c 'o', i 'm' ]
              }
            , { answer = "robot"
              , guess = "crown"
              , expected = [ i 'c', p 'r', p 'o', i 'w', i 'n' ]
              }
            , { answer = "robot"
              , guess = "robot"
              , expected = [ c 'r', c 'o', c 'b', c 'o', c 't' ]
              }

            --
            -- The following test is based on
            -- https://github.com/Kinkelin/WordleCompetition/pull/8
            --
            , { answer = "shine"
              , guess = "sense"
              , expected = [ c 's', i 'e', p 'n', i 's', c 'e' ]
              }

            --
            -- How does it handle when a letter occurs once
            -- but it is guessed multiple times?
            --
            , { answer = "apple" -- For e.g. 'e' occurs once.
              , guess = "beets" -- But, we guess it twice.

              -- Only the first 'e' should be possible.
              , expected = [ i 'b', p 'e', i 'e', i 't', i 's' ]
              }

            --
            -- More examples taken from
            -- https://dev.to/denvercoder1/why-most-wordle-clones-are-wrong-390c
            --
            , { answer = "those"
              , guess = "geese"
              , expected = [ i 'g', i 'e', i 'e', c 's', c 'e' ]
              }
            , { answer = "dread"
              , guess = "added"
              , expected = [ p 'a', p 'd', i 'd', p 'e', c 'd' ]
              }
            , { answer = "maxim"
              , guess = "mamma"
              , expected = [ c 'm', c 'a', p 'm', i 'm', i 'a' ]
              }
            ]


testGuess : { answer : String, guess : String, expected : Guess } -> Test
testGuess { answer, guess, expected } =
    test (Debug.toString { answer = answer, guess = guess }) <|
        \_ ->
            let
                maybeAnswer =
                    Answer.fromString dictionary answer

                maybeGuess =
                    Word.fromString dictionary guess

                maybeExpected =
                    Just expected

                maybeActual =
                    Maybe.map2 (Guess.guess Nothing) maybeAnswer maybeGuess
            in
            Expect.equal maybeActual maybeExpected


dictionary : Dictionary
dictionary =
    Dictionary.new
        { wordLength = 5
        , answers =
            [ "apple"
            , "dread"
            , "maxim"
            , "robot"
            , "shine"
            , "those"
            ]
        , nonAnswers =
            [ "added"
            , "beets"
            , "broom"
            , "crown"
            , "geese"
            , "mamma"
            , "sense"
            ]
        }


c : Char -> Letter
c =
    Letter.Correct


p : Char -> Letter
p =
    Letter.Possible


i : Char -> Letter
i =
    Letter.Impossible
