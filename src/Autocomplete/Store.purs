module Autocomplete.Store where

import Autocomplete.Types (Suggestion, Suggestions(..), SuggestionResults, Terms)
import Data.Map (Map, lookup, insert)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (Tuple(Tuple))
import Prelude (class Eq, (==), (&&))

-- | Stores searched terms so they can be recalled without re-querying.
-- | Also stores the current search terms.
newtype SuggesterState = SuggesterState { currentTerms :: Terms
                                        , currentResults :: Suggestions
                                        , previousResults :: Suggestions
                                        , store :: Map Terms Suggestions }

instance eqSuggestionStore :: Eq SuggesterState where
  eq (SuggesterState x) (SuggesterState y) = x.currentTerms == y.currentTerms
                                          && x.currentResults == y.currentResults

data SuggesterAction
  = SetTerms Terms
  | AddResults SuggestionResults

-- | Updates the suggestion store, updating either
-- | the current terms or search results.
updateSuggestions :: SuggesterAction -> SuggesterState -> SuggesterState
updateSuggestions action (SuggesterState state) =
  case action of
    SetTerms terms ->
      buildState terms state.currentResults state.store
    AddResults (Tuple terms results) ->
      let newStore = insert terms results state.store
      in buildState state.currentTerms state.previousResults newStore
  where
    buildState currentTerms previousResults store = SuggesterState
      { currentTerms
      , previousResults
      , store
      , currentResults: case lookup currentTerms store of
                          Nothing -> Loading (runSuggestions previousResults)
                          Just results -> results }
    runSuggestions :: Suggestions -> Array Suggestion
    runSuggestions (Loading a)  = a
    runSuggestions (Failed _ a) = a
    runSuggestions (Ready a)    = a

-- | Returns whether the store contains any form of results for the
-- | current terms.  Loading and failed search results are included.
hasSuggestionResults :: SuggesterState -> Boolean
hasSuggestionResults (SuggesterState s) =
  case lookup s.currentTerms s.store of
    Nothing -> false
    Just results ->
      case results of
        Ready _ -> true
        _       -> false

-- | Find the results for the store's current terms, or an empty result set.
getSuggestionResults :: SuggesterState -> Suggestions
getSuggestionResults (SuggesterState s) = s.currentResults
