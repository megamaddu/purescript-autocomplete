module Test.Autocomplete.Store where

import Prelude
import Autocomplete.Store (SuggesterState(SuggesterState), SuggesterAction(AddResults, SetTerms), updateSuggestions)
import Autocomplete.Types (Suggestions(Ready, Loading))
import Data.Map (singleton)
import Data.Monoid (mempty)
import Data.Tuple (Tuple(Tuple))
import Test.Unit (suite, test, TestSuite)
import Test.Unit.Assert (equal)

runTests :: forall e. TestSuite e
runTests =
  suite "Autocomplete.Store" do

    test "updateSuggestions returns empty Ready results for an empty store" do
      let
        emptyStateUnwrapped = (\(SuggesterState s) -> s) emptyState
      equal "" emptyStateUnwrapped.currentTerms
      equal (Ready []) emptyStateUnwrapped.currentResults

    test "updateSuggestions returns empty Loading results for an empty store after terms are set" do
      let
        updatedStateWrapped = updateSuggestions (SetTerms "foo") emptyState
        updatedState = (\(SuggesterState s) -> s) updatedStateWrapped
      equal "foo" updatedState.currentTerms
      equal (Loading []) updatedState.currentResults

    test "updateSuggestions returns empty Ready results for a store after terms are set and results loaded" do
      let
        fooResults = Ready ["a", "b", "c"]
        state1 = updateSuggestions (SetTerms "foo") emptyState
        state2 = updateSuggestions (AddResults (Tuple "foo" fooResults)) state1
        updatedState = (\(SuggesterState s) -> s) state2
      equal "foo" updatedState.currentTerms
      equal fooResults updatedState.currentResults

emptyState :: SuggesterState String
emptyState =
  SuggesterState
    { currentTerms: mempty
    , currentResults: mempty
    , termsHistory: mempty
    , store: singleton mempty mempty
    }
