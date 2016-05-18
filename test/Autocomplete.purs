module Test.Autocomplete where

import Prelude
import Signal.Channel as C
import Autocomplete (mkSuggester')
import Autocomplete.Api (SuggestionApi)
import Autocomplete.Types (Terms, Suggestions(Ready, Loading, Failed))
import Control.Monad.Aff (Aff, forkAff, later, later')
import Control.Monad.Eff (foreachE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF, readRef, writeRef, newRef)
import Data.Argonaut.Combinators ((.?))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Array (snoc)
import Data.Array.Unsafe (head, tail)
import Data.Either (Either)
import Network.HTTP.Affjax (AJAX)
import Signal.Channel (CHANNEL)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Unsafe.Coerce (unsafeCoerce)

runTests :: forall e.
  TestSuite
    ( channel :: CHANNEL
    , ajax :: AJAX
    , err :: EXCEPTION
    , ref :: REF
    | e
    )
runTests =
  suite "Autocomplete" do

    test "mkSuggester Send/Subscribe Integration Test" do
      resultsRef <- liftEff $ newRef []
      suggester <- liftEff $ mkSuggester' { api: fakeSuggestionApi 50
                                          , inputDebounce: 0.0
                                          , inputTransformer: id
                                          }
      liftEff $ suggester.subscribe \suggestions -> do
        results <- readRef resultsRef
        writeRef resultsRef $ snoc results suggestions
      let send = liftEff <<< suggester.send

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

      results <- liftEff $ readRef resultsRef

      let
        chevronExpectedMatches =
          [ Suggestion { phrase: "chevron infinity", hits: 1.1 }
          , Suggestion { phrase: "chevron print", hits: 1.0 }
          , Suggestion { phrase: "chevron infinity scarves", hits: 0.9 }
          ]

        tunicExpectedMatches =
          [ Suggestion { phrase: "tunics", hits: 0.3 }
          , Suggestion { phrase: "ruffled tunics", hits: 0.2 }
          , Suggestion { phrase: "striped tunics", hits: 0.1 }
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
        , Failed "Couldn't decode Array" [] -- "foo" done
        , Ready chevronExpectedMatches -- "chevron" done
        , Ready [] -- "" done
        , Loading [] -- "tunic" loading
        , Ready tunicExpectedMatches -- "tunic" done
        , Ready [] -- "" done
        , Ready chevronExpectedMatches -- "chevron" done
        ]
        results

newtype Suggestion = Suggestion { phrase :: String, hits :: Number }

derive instance eqSuggestion :: Eq Suggestion

instance decodeJsonSuggestion :: DecodeJson Suggestion where
  decodeJson json = do
    obj <- decodeJson json
    phrase <- obj .? "phrase"
    hits <- obj .? "weight"
    pure $ Suggestion { phrase, hits }

instance showSuggestion :: Show Suggestion where
  show (Suggestion x) = "{phrase: '" ++ x.phrase ++ "', hits: "    ++ show x.hits ++ "}"

fakeSuggestionApi :: Int -> SuggestionApi Suggestion
fakeSuggestionApi latency = { getSuggestions }
  where
    getSuggestions :: forall e.
      Terms -> Aff (ajax :: AJAX | e)
                   (Either
                      String
                      (Suggestions Suggestion))
    getSuggestions t = later' latency (pure $ decodeJson mockResults)
      where
        mockResults :: Json
        mockResults =
          case t of
            "chevron" ->
              unsafeCoerce
                [ { phrase: "chevron infinity", weight: 1.1 }
                , { phrase: "chevron print", weight: 1.0 }
                , { phrase: "chevron infinity scarves", weight: 0.9 }
                ]
            "tunic" ->
              unsafeCoerce
                [ { phrase: "tunics", weight: 0.3 }
                , { phrase: "ruffled tunics", weight: 0.2 }
                , { phrase: "striped tunics", weight: 0.1 }
                ]
            "foo" ->
              unsafeCoerce
                [ { pphrase: "chevron infinity", weight: 1 }
                , { pphrase: "chevron print", weight: 1 }
                , { pphrase: "chevron infinity scarves", weight: 0 }
                ]
            _ ->
              unsafeCoerce []

fromArr :: forall e a. Array a -> Aff ( channel :: C.CHANNEL | e ) (C.Channel a)
fromArr arr = do
  chan <- liftEff $ C.channel (head arr)
  forkAff $ later $ liftEff $ foreachE (tail arr) \a -> do
    C.send chan a
    pure unit
  pure chan

wait :: forall e. Int -> Aff e Unit
wait t = later' t $ pure unit
