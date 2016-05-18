module Test.Autocomplete.Store where

import Prelude
import Autocomplete.Store (SuggesterState(SuggesterState), SuggesterAction(AddResults, SetTerms), updateSuggestions)
import Autocomplete.Types (Terms, Suggestions(Ready, Loading))
import Data.Foldable (foldl)
import Data.Map (Map, singleton)
import Data.Monoid (mempty)
import Data.Tuple (Tuple(Tuple))
import Debug.Trace (spy)
import Test.Unit (failure, suite, test, TestSuite)
import Test.Unit.Assert (equal)

runTests :: forall e. TestSuite e
runTests =
  suite "Autocomplete.Store" do

    test "updateSuggestions returns empty Ready results for an empty store" do
      equal "" $ currentTerms emptyState
      equal (Ready []) $ currentResults emptyState

    test "updateSuggestions returns empty Loading results for an empty store after terms are set" do
      let
        updatedState = updateSuggestions (SetTerms "foo") emptyState
      equal "foo" $ currentTerms updatedState
      equal (Loading []) $ currentResults updatedState

    test "updateSuggestions returns empty Ready results for a store after terms are set and results loaded" do
      let
        fooResults = Ready ["a", "b", "c"]
        state1 = updateSuggestions (SetTerms "foo") emptyState
        state2 = updateSuggestions (AddResults (Tuple "foo" fooResults)) state1
      equal "foo" $ currentTerms state2
      equal fooResults $ currentResults state2

    test "updateSuggestions " do
      let
        ops =
          [ Tuple "f" (Ready ["a"])
          , Tuple "b" (Ready ["b"])
          , Tuple "fo" (Ready ["aa"])
          , Tuple "ba" (Ready ["bb"])
          , Tuple "foo" (Ready ["aaa"])
          , Tuple "bar" (Ready ["bbb"])
          ]
        state1 = foldl (flip $ AddResults >>> updateSuggestions) emptyState ops
      equal "" $ currentTerms state1
      failure "-- finish test --"

-- setTerms :: Terms -> Aff

currentTerms :: forall a. SuggesterState a -> Terms
currentTerms (SuggesterState s) = s.currentTerms

currentResults :: forall a. SuggesterState a -> Suggestions a
currentResults (SuggesterState s) = s.currentResults

currentStore :: forall a. SuggesterState a -> Map Terms (Suggestions a)
currentStore (SuggesterState s) = s.store

emptyState :: SuggesterState String
emptyState =
  SuggesterState
    { currentTerms: mempty
    , currentResults: mempty
    , termsHistory: mempty
    , store: singleton mempty mempty
    }
