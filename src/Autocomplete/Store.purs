module Autocomplete.Store where

import Prelude

import Autocomplete.Types (Suggestions(..), SuggestionResults, Terms)
import Data.Array (length)
import Data.List (List(Nil, Cons), (:), take)
import Data.Map (Map, alter, insert, lookup, update)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Tuple (Tuple(Tuple))
import Debug.Trace (spy)

-- | Stores searched terms so they can be recalled without re-querying.
-- | Also stores the current search terms.
newtype SuggesterState a = SuggesterState (Map Terms (Suggestions a))

data SuggesterAction a
  = AddResults (SuggestionResults a)
  | NoAction

-- | Updates the suggestion store, updating either
-- | the current terms or search results.
updateSuggestions :: forall a. SuggesterAction a -> SuggesterState a -> SuggesterState a
updateSuggestions action (SuggesterState store) =
  case spy "action" action of
    AddResults (Tuple terms results) ->
      let newStore = alter (maybeUpdateTermResult (spy ("update: " <> show terms) $ Just results)) terms store
      in SuggesterState newStore
    NoAction ->
      SuggesterState store
  where
    maybeUpdateTermResult = case _, _ of
      Just a             , Nothing          -> Just a -- always use incoming value when none exists
      Just a@(Failed _ _), _                -> Just a -- always use "result" values containing data
      Just a@(Ready _)   , _                -> Just a -- "
      _                  , b                -> b      -- otherwise, no update

getNextBestResults :: forall a. List Terms -> SuggesterState a -> Suggestions a
getNextBestResults Nil store = Loading []
getNextBestResults (Cons terms history) store =
  let results = getSuggestionResults terms store
  in case resultLength results of
    0 -> getNextBestResults history store
    _ -> results
  where
    resultLength (Loading xs) = length xs
    resultLength (Failed _ xs) = length xs
    resultLength (Ready xs) = length xs

-- | Returns whether the store contains any form of results for the
-- | current terms.
hasSuggestionResults :: forall a. Terms -> SuggesterState a -> Boolean
hasSuggestionResults terms (SuggesterState store) = isJust $ lookup terms store

-- | Find the results for the store's current terms, or an empty Loading result set.
getSuggestionResults :: forall a. Terms -> SuggesterState a -> Suggestions a
getSuggestionResults terms (SuggesterState store) =
  fromMaybe (Loading []) $ lookup terms store
