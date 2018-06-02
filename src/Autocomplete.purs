module Autocomplete
  ( mkSuggester
  , mkSuggester'
  , FetchFn
  , SuggesterSettings
  , SuggesterInstance
  ) where

import Prelude

import Autocomplete.Store (SuggesterState(SuggesterState), SuggesterAction(SetTerms, AddResults), hasSuggestionResults, getSuggestionResults, updateSuggestions)
import Autocomplete.Types (SuggestionResults, Suggestions(..), Terms)
import Data.Either (Either, either)
import Data.Map (singleton)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Exception (message)
import Signal ((~>), (<~), runSignal, dropRepeats, unwrap, foldp, merge)
import Signal.Channel (Channel, send, subscribe, channel)
import Signal.Time (debounce)

type FetchFn a = Terms -> Aff (Either String (Array a))

type SuggesterSettings a =
  { fetch :: FetchFn a
  , inputDebounce :: Milliseconds
  , inputTransformer :: Terms -> Terms
  }

-- | A suggester instance has a `send` function for providing new terms
-- | and a subscribe function for listening for new suggestions.
-- | The subscribe function will always be called with the most up to date
-- | suggestions for the latest terms sent, even if a previous search is
-- | slow to complete.  Subscribers immediately receive the most recent
-- | result set.
type SuggesterInstance a = Effect
  { send :: Terms -> Effect Unit
  , subscribe :: (Suggestions a -> Effect Unit) -> Effect Unit
  }

-- | Create a suggester with the default API backend: Affjax.get & decodeJson,
-- | no input debounce, and no input transformations.
mkSuggester :: forall a . Eq a => FetchFn a -> SuggesterInstance a
mkSuggester fetch = mkSuggester'
  { fetch
  , inputDebounce: Milliseconds 0.0
  , inputTransformer: identity
  }

-- | Create a suggester with an alternate API backend.
mkSuggester' :: forall a. Eq a => SuggesterSettings a -> SuggesterInstance a
mkSuggester' settings = do
  termChan <- channel mempty
  searchResChan <- channel mempty
  let
    terms = SetTerms <~ dropRepeats
      ( debounce (case settings.inputDebounce of Milliseconds s -> s)
        $ subscribe termChan
      )
    searchRes = AddResults <~ subscribe searchResChan
    suggesterActions = merge terms searchRes
    storeFoldp = foldp updateSuggestions initialStore suggesterActions
  stores <- unwrap $ storeFoldp
                  ~> \store -> do runSearch settings.fetch searchResChan store
                                  pure store
  let output = dropRepeats $ getSuggestionResults <~ stores
  pure { send: send termChan
       , subscribe: \cb -> runSignal (output ~> cb)
       }
  where
    initialStore = SuggesterState
      { currentTerms: mempty
      , currentResults: mempty
      , termsHistory: mempty
      , store: singleton mempty mempty
      }

-- | Internal function for running the fetch function when needed.
runSearch
  :: forall a
   . FetchFn a
  -> Channel (SuggestionResults a)
  -> (SuggesterState a)
  -> Effect Unit
runSearch fetch chan st@(SuggesterState store) = do
  if terms == mempty || hasSuggestionResults st
    then pure unit
    else void do
      send chan $ Tuple terms $ Loading []
      runAff_ (either handleAjaxError handleParseResults) (fetch terms)
  where
    terms = store.currentTerms
    handleAjaxError e = send chan $ Tuple terms $ Failed (message e) []
    handleParseResults e = send chan $ Tuple terms results
      where results = either (\msg -> Failed msg []) Ready e
 
