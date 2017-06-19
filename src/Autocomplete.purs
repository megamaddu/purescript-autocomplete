module Autocomplete
  ( mkSuggester
  , mkSuggester'
  , SuggesterSettings
  , SuggesterEffects
  , SuggesterInstance
  ) where

import Prelude

import Autocomplete.Api (SuggestionApi, mkDefaultApi)
import Autocomplete.Store (SuggesterState(SuggesterState), SuggesterAction(SetTerms, AddResults), hasSuggestionResults, getSuggestionResults, updateSuggestions)
import Autocomplete.Types (Terms, SuggestionResults, Suggestions(Loading, Failed))
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, message)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Either (either)
import Data.Map (singleton)
import Data.Monoid (mempty)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(Tuple))
import Network.HTTP.Affjax (AJAX)
import Signal ((~>), (<~), runSignal, dropRepeats, unwrap, foldp, merge)
import Signal.Channel (CHANNEL, Channel, send, subscribe, channel)
import Signal.Time (debounce)

type SuggesterSettings a =
  { api :: SuggestionApi a
  , inputDebounce :: Milliseconds
  , inputTransformer :: Terms -> Terms
  }

-- | All effects induced by a suggester during its lifespan.
type SuggesterEffects e a =
  Eff
    ( channel :: CHANNEL
    , ajax :: AJAX
    , err :: EXCEPTION
    | e
    ) a

-- | A suggester instance has a `send` function for providing new terms
-- | and a subscribe function for listening for new suggestions.
-- | The subscribe function will always be called with the most up to date
-- | suggestions for the latest terms sent, even if a previous search is
-- | slow to complete.  Subscribers immediately receive the most recent
-- | result set.
type SuggesterInstance e a = SuggesterEffects e
  { send :: Terms -> SuggesterEffects e Unit
  , subscribe :: (Suggestions a -> SuggesterEffects e Unit)
                  -> SuggesterEffects e Unit
  }

-- | Create a suggester with the default API backend: Affjax.get & decodeJson,
-- | no input debounce, and no input transformations.
mkSuggester :: forall e a. Eq a => DecodeJson a => String -> SuggesterInstance e a
mkSuggester baseUri = mkSuggester'
  { api: mkDefaultApi baseUri
  , inputDebounce: Milliseconds 0.0
  , inputTransformer: id
  }

-- | Create a suggester with an alternate API backend.
mkSuggester' :: forall e a. Eq a => SuggesterSettings a -> SuggesterInstance e a
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
                  ~> \store -> do runSearch settings.api searchResChan store
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

-- | Internal function for running the API backend and decoding results.
runSearch :: forall e a.
  SuggestionApi a
  -> Channel (SuggestionResults a)
  -> (SuggesterState a)
  -> Eff
      ( channel :: CHANNEL
      , ajax :: AJAX
      , err :: EXCEPTION
      | e
      ) Unit
runSearch api chan st@(SuggesterState store) = do
  if terms == mempty || hasSuggestionResults st
    then pure unit
    else void do
      send chan $ Tuple terms $ Loading []
      runAff handleAjaxError handleParseResults (api.getSuggestions terms)
  where
    terms = store.currentTerms
    handleAjaxError e = send chan $ Tuple terms $ Failed (message e) []
    handleParseResults e = send chan $ Tuple terms results
      where results = either (\msg -> Failed msg []) id e
 
