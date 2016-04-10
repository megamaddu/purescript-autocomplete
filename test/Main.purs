module Test.Main where

import Autocomplete (mkSuggester')
import Autocomplete.Api (SuggestionApi)
import Autocomplete.Types (Terms, Suggestion(Suggestion), Suggestions(Ready, Loading, Failed))
import Control.Monad.Aff (Aff, forkAff, later, later')
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (readRef, writeRef, newRef)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (snoc)
import Data.Array.Unsafe (head, tail)
import Data.Either (Either)
import Network.HTTP.Affjax (AJAX)
import Prelude
import Signal.Channel as C
import Signal.Time (debounce, since)
import Test.Signal (expect)
import Test.Unit (test, runTest)
import Test.Util (wait)
import Test.Unit.Assert (equal)
import Unsafe.Coerce (unsafeCoerce)

main :: Eff _ Unit
main = runTest do
  test "Autocomplete Send/Subscribe Integration Test" do
    resultsRef <- liftEff $ newRef []
    suggester <- liftEff $ mkSuggester' $ { api: fakeSuggestionApi 50
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
    send "fooo"
    wait 200

    results <- liftEff $ readRef resultsRef

    let expectedMatches = [ Suggestion { phrase: "chevron infinity", hits: 1.1 }
                          , Suggestion { phrase: "chevron print", hits: 1.0 }
                          , Suggestion { phrase: "chevron infinity scarves", hits: 0.9 }
                          ]
    equal [ Ready [] -- initial
          , Loading [] -- "c" to "chevron" loading
          , Ready expectedMatches -- "chevron" done
          , Loading expectedMatches -- "chevronx" loading
          , Ready expectedMatches -- "chevronx" done
          -- , Ready expectedMatches -- "chevron" done -- skipped because output doesn't change
          -- , Ready expectedMatches -- "chevr" done -- skipped because output doesn't change
          , Ready [] -- "" done
          , Loading [] -- "foo" loading, "fooo" Loading
          , Ready [] -- "fooo" done
          , Failed "Couldn't decode Array" [] -- "foo" done
          , Ready [] -- "fooo" done
          ]
          results

fakeSuggestionApi :: Int -> SuggestionApi
fakeSuggestionApi latency = { getSuggestions }
  where
    getSuggestions :: forall e. Terms -> Aff ( ajax :: AJAX
                                             | e
                                             ) (Either String Suggestions)
    getSuggestions t = later' latency $
      pure $ decodeJson mockResults
      where
        mockResults :: Json
        mockResults =
          case t of
            "chevron" -> unsafeCoerce [ { phrase: "chevron infinity", weight: 1.1 }
                                      , { phrase: "chevron print", weight: 1.0 }
                                      , { phrase: "chevron infinity scarves", weight: 0.9 }
                                      ]
            "foo" -> unsafeCoerce [ { pphrase: "chevron infinity", weight: 1 }
                                  , { pphrase: "chevron print", weight: 1 }
                                  , { pphrase: "chevron infinity scarves", weight: 0 }
                                  ]
            _ -> unsafeCoerce []

fromArr :: forall e a. Array a -> Aff ( channel :: C.CHANNEL | e ) (C.Channel a)
fromArr arr = do
  chan <- liftEff $ C.channel (head arr)
  forkAff $ later $ liftEff $ foreachE (tail arr) \a -> do
    C.send chan a
    pure unit
  pure chan
