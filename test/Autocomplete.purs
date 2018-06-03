module Test.Autocomplete where

import Prelude

import Autocomplete (FetchFn, mkSuggester')
import Autocomplete.Types (Suggestions(Ready, Loading, Failed))
import Data.Array (snoc)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

runTests :: TestSuite
runTests =
  suite "Autocomplete" do

    test "mkSuggester Send/Subscribe Integration Test" do
      suggester <- liftEffect $ mkSuggester'
        { fetch: fakeFetch (Milliseconds 50.0)
        , inputDebounce: Milliseconds 0.0
        , inputTransformer: identity
        }
      resultsRef <- liftEffect $ Ref.new []
      liftEffect $ suggester.subscribe \suggestions -> do
        Ref.modify_ (_ `snoc` suggestions) resultsRef

      let
        send = liftEffect <<< suggester.send
        wait = delay <<< Milliseconds <<< toNumber

      wait 30
      send "c"
      wait 30
      send "ch"
      wait 30
      send "che"
      wait 30
      send "chev"
      wait 30
      send "chevr"
      wait 30
      send "chevro"
      wait 30
      send "chevron"
      wait 200
      send "chevronx"
      wait 200
      send "chevron"
      wait 200
      send "chevr"
      wait 200
      send ""
      wait 200
      send "foo"
      wait 30
      send "fooo"
      wait 200
      send "foo"
      wait 200
      send "chevron"
      wait 200
      send ""
      wait 200
      send "tunic"
      wait 200
      send ""
      wait 200
      send "chevron"
      wait 200

      results <- liftEffect $ Ref.read resultsRef

      let
        chevronExpectedMatches =
          [ { phrase: "chevron infinity", hits: 1.1 }
          , { phrase: "chevron print", hits: 1.0 }
          , { phrase: "chevron infinity scarves", hits: 0.9 }
          ]

        tunicExpectedMatches =
          [ { phrase: "tunics", hits: 0.3 }
          , { phrase: "ruffled tunics", hits: 0.2 }
          , { phrase: "striped tunics", hits: 0.1 }
          ]

      equal
        [ Ready [] -- initial
        , Loading [] -- "c" to "chevron" loading
        , Ready chevronExpectedMatches -- "chevron" done
        , Loading chevronExpectedMatches -- "chevronx" loading
        , Ready chevronExpectedMatches -- "chevronx" done
        -- , Ready chevronExpectedMatches -- "chevron" done -- skipped because output doesn't change
        -- , Ready chevronExpectedMatches -- "chevr" done -- skipped because output doesn't change
        , Ready [] -- "" done
        , Loading [] -- "foo" loading, "fooo" Loading
        , Ready [] -- "fooo" done
        , Failed "It broke" [] -- "foo" done
        , Ready chevronExpectedMatches -- "chevron" done
        , Ready [] -- "" done
        , Loading [] -- "tunic" loading
        , Ready tunicExpectedMatches -- "tunic" done
        , Ready [] -- "" done
        , Ready chevronExpectedMatches -- "chevron" done
        ]
        results

type Suggestion = { phrase :: String, hits :: Number }

fakeFetch :: Milliseconds -> FetchFn Suggestion
fakeFetch latency terms = do
      delay latency
      pure mockResults
      where
        mockResults =
          case terms of
            "chevron" ->
              Right
                [ { phrase: "chevron infinity", hits: 1.1 }
                , { phrase: "chevron print", hits: 1.0 }
                , { phrase: "chevron infinity scarves", hits: 0.9 }
                ]
            "tunic" ->
              Right
                [ { phrase: "tunics", hits: 0.3 }
                , { phrase: "ruffled tunics", hits: 0.2 }
                , { phrase: "striped tunics", hits: 0.1 }
                ]
            "foo" ->
              Left "It broke"
            _ ->
              Right []
