module Test.Main where

import Autocomplete
import Autocomplete.Api
import Autocomplete.Store
import Autocomplete.Types
import Control.Monad.Aff (Aff, makeAff, forkAff, later, later')
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (foreachE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Data.Argonaut.Core (Json)
import Data.Array (snoc)
import Data.Array.Unsafe (head, tail)
import Data.Function (Fn4, runFn4)
import Data.List (List(..), toList, fromList)
import Data.Monoid (class Monoid, mempty)
import Network.HTTP.StatusCode (StatusCode(StatusCode))
import Prelude (class Show, class Eq, Unit, ($), (==), (<<<), bind, id, pure, unit, const, show, (++), (/=))
import Signal (Signal, constant, (~>), runSignal)
import Signal.Channel as C
import Signal.Time (since)
import Test.Signal (expect, expectFn)
import Test.Unit (Assertion, Test, test, timeout, runTest)
import Test.Util (wait)
import Test.Unit.Assert (equal)
import Unsafe.Coerce (unsafeCoerce)
import Util.Signal

main = runTest do
  test "Autocomplete Send/Subscribe Integration Test" do
    resultsRef <- liftEff $ newRef []
    suggester <- liftEff $ mkSuggester' $ { api: fakeSuggestionApi 50
                                          , inputDebounce: 100.0
                                          , inputTransformer: id
                                          }
    liftEff $ suggester.subscribe \suggestions -> do
      results <- readRef resultsRef
      writeRef resultsRef $ snoc results suggestions
    let send = liftEff <<< suggester.send

    wait 30
    send "ch"
    wait 30
    send "chevron"
    wait 200
    send "chevronx"
    wait 200
    send "chevron"
    wait 200

    results <- liftEff $ readRef resultsRef

    let expectedMatches = [ Suggestion { phrase: "chevron infinity", hits: 1 }
                          , Suggestion { phrase: "chevron print", hits: 1 }
                          , Suggestion { phrase: "chevron infinity scarves", hits: 1 } ]
    equal [ Ready []
          , Loading []
          , Ready expectedMatches
          , Loading expectedMatches
          , Ready expectedMatches
          -- , Ready expectedMatches -- this gets skipped because the output doesn't change
          ]
          results

  test "Util.Signal.whenEqual" do
    chan <- fromArr [1,2,3,4,3,2,1,2,3,3,2,1,4]
    let sig = whenEqual 3 $ C.subscribe chan
    expect 50 sig [3,3,3,3]

  test "Util.Signal.whenChangeTo" do
    chan <- fromArr [1,2,3,4,3,2,1,2,3,3,2,1,4]
    let sig = whenChangeTo 3 $ C.subscribe chan
    expect 50 sig [3,3,3]

  test "Util.Signal.since" do
    chan <- liftEff $ C.channel 0
    let sig = since 10.0 $ C.subscribe chan
        send = liftEff <<< C.send chan

    forkAff $ expect 50 sig [false,true,false,true,false]
    wait 20
    send 1
    wait 20
    send 2
    wait 20

  test "Util.Signal.debounce" do
    chan <- liftEff $ C.channel 0
    let sig = debounce 10.0 $ C.subscribe chan
        send = liftEff <<< C.send chan

    forkAff $ expect 50 sig [0,2,4]
    wait 20
    send 1
    wait 5
    send 2
    wait 20
    send 3
    wait 5
    send 4
    wait 20

fakeSuggestionApi latency = { getSuggestions }
  where
    getSuggestions t = later' latency $
      pure { status: StatusCode 200
           , headers: []
           , response: mockResults }
      where
        mockResults :: Json
        mockResults = unsafeCoerce
          case t of
            "chevron" -> [ { phrase: "chevron infinity", weight: 1 }
                         , { phrase: "chevron print", weight: 1 }
                         , { phrase: "chevron infinity scarves", weight: 1 } ]
            _ -> []

fromArr :: forall e a. Array a -> Aff ( channel :: C.CHANNEL | e ) (C.Channel a)
fromArr arr = do
  chan <- liftEff $ C.channel (head arr)
  forkAff $ later $ liftEff $ foreachE (tail arr) \a -> do
    C.send chan a
    pure unit
  pure chan
