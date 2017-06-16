module Test.Main where

import Prelude
import Signal.Channel as C
import Autocomplete (mkSuggester')
import Autocomplete.Api (SuggestionApi)
import Autocomplete.Types (Terms, Suggestions(Ready, Loading, Failed))
import Control.Monad.Aff (Aff, forkAff, later, later')
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF, modifyRef, newRef, readRef, writeRef)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Array (snoc)
import Data.Array.Partial (head, tail)
import Data.Either (Either)
import Network.HTTP.Affjax (AJAX)
import Partial.Unsafe (unsafePartial)
import Signal.Channel (CHANNEL)
import Test.Unit (test)
import Test.Unit.Assert (equal)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Test.Util (wait)
import Unsafe.Coerce (unsafeCoerce)

main :: Eff
  ( console :: CONSOLE
  , testOutput :: TESTOUTPUT
  , avar :: AVAR
  , ref :: REF
  , channel :: CHANNEL
  , ajax :: AJAX
  , err :: EXCEPTION
  ) Unit
main = runTest do
  test "Autocomplete Send/Subscribe Integration Test" do
    resultsRef <- liftEff $ newRef []
    let fakeApi = fakeSuggestionApi 50
    suggester <- liftEff $ mkSuggester' $
                  { api: fakeApi.api
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
          , Failed "Couldn't decode Array: Expected field \"phrase\"" [] -- "foo" done
          , Ready [] -- "fooo" done
          ]
          results
    log =<< show <$> liftEff fakeApi.getRequestCount

    equal true false

newtype Suggestion = Suggestion { phrase :: String, hits :: Number }

derive instance eqSuggestion :: Eq Suggestion

instance decodeJsonSuggestion :: DecodeJson Suggestion where
  decodeJson json = do
    obj <- decodeJson json
    phrase <- obj .? "phrase"
    hits <- obj .? "weight"
    pure $ Suggestion { phrase, hits }

instance showSuggestion :: Show Suggestion where
  show (Suggestion x) = "{phrase: '" <> x.phrase <> "', hits: " <> show x.hits <> "}"

fakeSuggestionApi :: Int -> { api :: SuggestionApi Suggestion, getRequestCount :: forall e. Eff (ref :: REF | e) Int }
fakeSuggestionApi latency =
  { api: { getSuggestions }
  , getRequestCount
  }
  where
    counterRef = unsafePerformEff $ newRef 0

    getSuggestions :: forall e.
      Terms
      -> Aff
          ( ajax :: AJAX
          | e
          ) (Either String (Suggestions Suggestion))
    getSuggestions t = do
      let unused = unsafePerformEff $ modifyRef counterRef (_ + 1)
      later' latency $ pure $ decodeJson mockResults
      where
        mockResults :: Json
        mockResults =
          case t of
            "chevron" -> unsafeCoerce
              [ { phrase: "chevron infinity", weight: 1.1 }
              , { phrase: "chevron print", weight: 1.0 }
              , { phrase: "chevron infinity scarves", weight: 0.9 }
              ]
            "foo" -> unsafeCoerce
              [ { pphrase: "chevron infinity", weight: 1 }
              , { pphrase: "chevron print", weight: 1 }
              , { pphrase: "chevron infinity scarves", weight: 0 }
              ]
            _ -> unsafeCoerce []
    
    getRequestCount = readRef counterRef

fromArr :: forall e a. Array a -> Aff ( channel :: C.CHANNEL | e ) (C.Channel a)
fromArr arr = unsafePartial do
  chan <- liftEff $ C.channel (head arr)
  forkAff $ later $ liftEff $ foreachE (tail arr) \a -> do
    C.send chan a
    pure unit
  pure chan
