module Autocomplete
  ( mkSuggester
  , mkSuggester'
  , SuggesterSettings
  , SuggesterEffects
  , SuggesterInstance
  ) where

import Autocomplete.Api (SuggestionApi, mkDefaultApi)
import Autocomplete.Store (SuggesterState(SuggesterState), SuggesterAction(SetTerms, AddResults), hasSuggestionResults, getSuggestionResults, updateSuggestions)
import Autocomplete.Types (Terms, SuggestionResults, Suggestions(Failed))
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, message)
import Data.Either (either)
import Data.Map (insert)
import Data.Monoid (mempty)
import Data.Tuple (Tuple(Tuple))
import Network.HTTP.Affjax (AJAX)
import Prelude
import Signal ((~>), (<~), runSignal, dropRepeats, unwrap, foldp, merge)
import Signal.Channel (CHANNEL, Channel, send, subscribe, channel)
import Util.Signal (debounce)

type SuggesterSettings = { api :: SuggestionApi
                         , inputDebounce :: Number
                         , inputTransformer :: Terms -> Terms
                         }

-- | All effects induced by a suggester during its lifespan.
type SuggesterEffects e a = Eff ( channel :: CHANNEL
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
type SuggesterInstance e = SuggesterEffects e
  { send :: Terms -> SuggesterEffects e Unit
  , subscribe :: (Suggestions -> SuggesterEffects e Unit)
              -> SuggesterEffects e Unit
  }

-- | Create a suggester with the default API backend: Affjax.get & decodeJson,
-- | no input debounce, and no input transformations.
mkSuggester :: forall e. String -> SuggesterInstance e
mkSuggester baseUri = mkSuggester' { api: mkDefaultApi baseUri
                                   , inputDebounce: 0.0
                                   , inputTransformer: id
                                   }

-- | Create a suggester with an alternate API backend.
mkSuggester' :: forall e. SuggesterSettings -> SuggesterInstance e
mkSuggester' settings = do
  termChan <- channel mempty
  searchResChan <- channel mempty
  let terms = SetTerms <~ dropRepeats (debounce settings.inputDebounce
                                              $ subscribe termChan)
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
    initialStore :: SuggesterState
    initialStore = SuggesterState { currentTerms: mempty
                                  , currentResults: mempty
                                  , termsHistory: mempty
                                  , store: insert mempty mempty mempty
                                  }

-- | Internal function for running the API backend and decoding results.
runSearch :: forall e.
             SuggestionApi
          -> Channel SuggestionResults
          -> SuggesterState
          -> Eff ( channel :: CHANNEL
                 , ajax :: AJAX
                 , err :: EXCEPTION
                 | e
                 ) Unit
runSearch api chan st@(SuggesterState store) = do
  if terms == mempty || hasSuggestionResults st
    then pure unit
    else runAff handleAjaxError handleParseResults (api.getSuggestions terms)
  where
    terms = store.currentTerms
    handleAjaxError e = send chan $ Tuple terms $ Failed (message e) []
    handleParseResults e = do
      let results = either (\msg -> Failed msg []) id e
      send chan $ Tuple terms results
