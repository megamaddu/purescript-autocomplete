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
  case action of
    AddResults (Tuple terms results) ->
      -- let newStore = alter (maybeUpdateTermResult $ Just results) terms store
      let newStore = insert terms results store
      in SuggesterState newStore
    NoAction ->
      SuggesterState store
  where
    maybeUpdateTermResult = case _, _ of
      Just a             , Nothing          -> Just a -- always use incoming value when none exists
      Just a@(Failed _ _), _                -> Just a -- always use "result" values containing data
      Just a@(Ready _)   , _                -> Just a -- "
      _                  , b                -> b      -- otherwise, no update

-- | Returns whether the store contains any form of results for the
-- | current terms.
hasSuggestionResults :: forall a. Terms -> SuggesterState a -> Boolean
hasSuggestionResults terms (SuggesterState store) = isJust $ lookup terms store

-- | Find the results for the store's current terms, or an empty Loading result set.
getSuggestionResults :: forall a. List Terms -> SuggesterState a -> Suggestions a
getSuggestionResults Nil                  _                      = spy "no-terms" $ Loading []
getSuggestionResults (Cons terms history) (SuggesterState store) =
  case lookup terms store of
    Nothing       -> substitute (runSuggestions $ getFirstResult history (SuggesterState store)) (Loading [])
    Just results' -> results'
    -- Nothing       -> spy (show terms <> "--substituting") $ substitute (runSuggestions $ getFirstResult history (SuggesterState store)) (Loading [])
    -- Just results' -> spy (show terms) results'

getFirstResult :: forall a. List Terms -> SuggesterState a -> Suggestions a
getFirstResult Nil                  _                      = Loading []
getFirstResult (Cons terms history) (SuggesterState store) =
  case lookup terms store of
    Nothing       -> getFirstResult history (SuggesterState store)
    Just results' -> results'
    Just results' -> spy (">>" <> show terms) results'

substitute :: forall a. Array a -> Suggestions a -> Suggestions a
substitute xs = case _ of
  Loading _ -> Loading xs
  Failed error _ -> Failed error xs
  Ready _ -> Ready xs

runSuggestions :: forall a. Suggestions a -> Array a
runSuggestions = case _ of
  Loading xs -> xs
  Failed _ xs -> xs
  Ready xs -> xs
