module Autocomplete.Store where

import Prelude

import Data.Array (length)
import Data.List (List(Cons, Nil), (:), take)
import Data.Map (Map, lookup, insert)
import Data.Maybe (isJust, fromMaybe)
import Data.Tuple (Tuple(Tuple))

import Autocomplete.Types (Suggestion, Suggestions(..), SuggestionResults, Terms)

-- | Stores searched terms so they can be recalled without re-querying.
-- | Also stores the current search terms.
newtype SuggesterState = SuggesterState { currentTerms :: Terms
                                        , currentResults :: Suggestions
                                        , termsHistory :: List Terms
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
      let newHistory = state.currentTerms : state.termsHistory
      in buildState terms newHistory state.store
    AddResults (Tuple terms results) ->
      let newStore = insert terms results state.store
      in buildState state.currentTerms state.termsHistory newStore
  where
    buildState currentTerms termsHistory store = SuggesterState
      { currentTerms
      , termsHistory: take 100 termsHistory
      , store
      , currentResults:
        let results = lookupOrLoading currentTerms store
         in case length (runSuggestions results) of
                 0 -> results `substitute` (getNextBestResults termsHistory store)
                 _ -> results
      }

    runSuggestions :: Suggestions -> Array Suggestion
    runSuggestions (Loading a)  = a
    runSuggestions (Failed _ a) = a
    runSuggestions (Ready a)    = a

    substitute :: Suggestions -> Array Suggestion -> Suggestions
    substitute (Loading _)  r = Loading r
    substitute (Failed e _) r = Failed e r
    substitute (Ready _)    r = Ready r

    lookupOrLoading :: Terms -> Map Terms Suggestions -> Suggestions
    lookupOrLoading terms store = fromMaybe (Loading []) $ lookup terms store

    getNextBestResults :: List Terms -> Map Terms Suggestions -> Array Suggestion
    getNextBestResults Nil store = []
    getNextBestResults (Cons terms history) store =
      let results = runSuggestions $ lookupOrLoading terms store
       in case length results of
               0 -> getNextBestResults history store
               _ -> results

-- | Returns whether the store contains any form of results for the
-- | current terms.
hasSuggestionResults :: SuggesterState -> Boolean
hasSuggestionResults (SuggesterState s) = isJust $ lookup s.currentTerms s.store

-- | Find the results for the store's current terms, or an empty result set.
getSuggestionResults :: SuggesterState -> Suggestions
getSuggestionResults (SuggesterState s) = s.currentResults
