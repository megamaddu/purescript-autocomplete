module Test.Autocomplete where

import Prelude

import Autocomplete (FetchFn, mkSuggester')
import Autocomplete.Types (Suggestions(Ready, Loading, Failed))
import Data.Array (snoc)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse, traverse_)
import Effect.Aff (delay)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

runTests :: TestSuite
runTests =
  suite "Autocomplete" do

    -- test "mkSuggester Send/Subscribe Asynchronous Integration Test" do
    --   suggester <- liftEffect $ mkSuggester'
    --     { fetch: fakeFetch (pure $ Milliseconds 50.0)
    --     , inputDebounce: Nothing
    --     }
    --   resultsRef <- liftEffect $ Ref.new []
    --   liftEffect $ suggester.subscribe \suggestions -> do
    --     Ref.modify_ (_ `snoc` suggestions) resultsRef

    --   let
    --     send = liftEffect <<< suggester.send
    --     wait = delay <<< Milliseconds <<< toNumber

    --   send ""
    --   wait 30
    --   send "c"
    --   wait 30
    --   send "ch"
    --   wait 30
    --   send "che"
    --   wait 30
    --   send "chev"
    --   wait 30
    --   send "chevr"
    --   wait 30
    --   send "chevro"
    --   wait 30
    --   send "chevron"
    --   wait 200
    --   send "chevronx"
    --   wait 200
    --   send "chevron"
    --   wait 200
    --   send "chevr"
    --   wait 200
    --   send ""
    --   wait 200
    --   send "foo"
    --   wait 30
    --   send "fooo"
    --   wait 200
    --   send "foo"
    --   wait 200
    --   send "chevron"
    --   wait 200
    --   send ""
    --   wait 200
    --   send "tunic"
    --   wait 200
    --   send ""
    --   wait 200
    --   send "chevron"
    --   wait 200
    --   send "tunic"
    --   wait 200

    --   results <- liftEffect $ Ref.read resultsRef

    --   let
    --     chevronExpectedMatches =
    --       [ { phrase: "chevron infinity" }
    --       , { phrase: "chevron print" }
    --       , { phrase: "chevron infinity scarves" }
    --       ]

    --     tunicExpectedMatches =
    --       [ { phrase: "tunics" }
    --       , { phrase: "ruffled tunics" }
    --       , { phrase: "striped tunics" }
    --       ]

    --   equal
    --     [ Loading [] -- initial subscribe
    --     , Ready [] -- "" synchronously completes
    --     , Loading [] -- "c" to "chevron" loading
    --     , Ready chevronExpectedMatches -- "chevron" done
    --     , Loading chevronExpectedMatches -- "chevronx" loading
    --     , Ready [] -- "chevronx" done
    --     , Ready chevronExpectedMatches -- "chevron" done
    --     -- , Ready chevronExpectedMatches -- "chevr" done -- skipped because output doesn't change
    --     , Ready [] -- "" done
    --     , Loading [] -- "foo" loading, "fooo" Loading
    --     , Ready [] -- "fooo" done
    --     , Failed "It broke" [] -- "foo" done
    --     , Ready chevronExpectedMatches -- "chevron" done
    --     , Ready [] -- "" done
    --     , Loading [] -- "tunic" loading
    --     , Ready tunicExpectedMatches -- "tunic" done
    --     , Ready [] -- "" done
    --     , Ready chevronExpectedMatches -- "chevron" done
    --     , Ready tunicExpectedMatches -- "tunic" done
    --     ]
    --     results

    test "mkSuggester Send/Subscribe Synchronous Empty State Integration Test" do
      suggester <- liftEffect $ mkSuggester'
        { fetch: fakeFetch Nothing
        , inputDebounce: Nothing
        }
      resultsRef <- liftEffect $ Ref.new []
      liftEffect $ suggester.subscribe \suggestions -> do
        Ref.modify_ (_ `snoc` suggestions) resultsRef
      results <- liftEffect $ Ref.read resultsRef

      equal
        [
        -- [ Ready [] -- synchronous initial "" value
        ]
        results

    test "mkSuggester Send/Subscribe Synchronous Integration Test" do
      suggester <- liftEffect $ mkSuggester'
        { fetch: fakeFetch Nothing
        , inputDebounce: Nothing
        }
      resultsRef <- liftEffect $ Ref.new []
      liftEffect $ suggester.subscribe \suggestions -> do
        Ref.modify_ (_ `snoc` suggestions) resultsRef

      let
        wait = delay <<< Milliseconds <<< toNumber
        send term = wait 10 <* (liftEffect $ suggester.send $ term)

      -- "wait" is still necessary here to allow
      -- values to be processed, just as they normally
      -- would be between a user's key strokes. The
      -- difference here from the async version is
      -- that we should not see "Loading _" in the
      -- results.
      send "c"
      send "ch"
      send "che"
      send "chev"
      send "chevr"
      send "chevro"
      send "chevron"
      -- send "chevronx"
      -- send "chevron"
      -- send "chevr"
      -- send ""
      -- send "foo"
      -- send "fooo"
      -- send "foo"
      -- send "chevron"
      -- send ""
      -- send "tunic"
      -- send ""
      -- send "chevron"
      -- send "tunic"
      -- wait 10

      results <- liftEffect $ Ref.read resultsRef

      equal
        [ Ready [] -- synchronous initial "" value
        , Ready chevronExpectedMatches -- "chevron"
        -- , Ready [] -- "chevronx"
        -- , Ready chevronExpectedMatches -- "chevron" again
        -- , Ready [] -- "chevr", "" again
        -- , Failed "It broke" [] -- "foo"
        -- , Ready [] -- "fooo"
        -- , Failed "It broke" [] -- "foo" again
        -- , Ready chevronExpectedMatches -- "chevron" again
        -- , Ready [] -- "" again
        -- , Ready tunicExpectedMatches -- "tunic"
        -- , Ready [] -- ""
        -- , Ready chevronExpectedMatches -- "chevron"
        -- , Ready tunicExpectedMatches -- "tunic"
        ]
        results

  where
    chevronExpectedMatches =
      [ { phrase: "chevron infinity" }
      , { phrase: "chevron print" }
      , { phrase: "chevron infinity scarves" }
      ]

    tunicExpectedMatches =
      [ { phrase: "tunics" }
      , { phrase: "ruffled tunics" }
      , { phrase: "striped tunics" }
      ]

type Suggestion = { phrase :: String }

fakeFetch :: Maybe Milliseconds -> FetchFn Suggestion
fakeFetch _ "" = pure $ Right []
fakeFetch latency terms = do
      traverse_ delay latency
      pure mockResults
      where
        mockResults =
          case terms of
            "chevron" ->
              Right
                [ { phrase: "chevron infinity" }
                , { phrase: "chevron print" }
                , { phrase: "chevron infinity scarves" }
                ]
            "tunic" ->
              Right
                [ { phrase: "tunics" }
                , { phrase: "ruffled tunics" }
                , { phrase: "striped tunics" }
                ]
            "foo" ->
              Left "It broke"
            _ ->
              Right []
