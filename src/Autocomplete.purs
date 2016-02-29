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
import Control.Monad.Aff (Aff, launchAff, attempt, liftEff')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, Error, message)
import Data.Argonaut.Decode (decodeJson)
import Data.Either (Either(Left, Right), either)
import Data.Map (insert)
import Data.Monoid (mempty)
import Data.Tuple (Tuple(Tuple))
import Network.HTTP.Affjax (AJAX)
import Prelude (Unit, ($), (<$>), (==), (||), (<<<), bind, pure, unit, id)
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

-- | Create a suggester with the default API backend.
mkSuggester :: forall e. String -> SuggesterInstance e
mkSuggester baseUri = mkSuggester' { api: mkDefaultApi baseUri
                                   , inputDebounce: 100.0
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
                                  , previousResults: mempty
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
  let terms = store.currentTerms
  if terms == mempty || hasSuggestionResults st
     then pure unit
     else launchAff do
       res <- handleSearchError <$> search api terms
       liftEff' $ send chan $ Tuple terms res
  where
    unwrapMessage :: forall a. Either Error a -> Either String a
    unwrapMessage = either (Left <<< message) Right

    handleError :: Either String Suggestions -> Suggestions
    handleError = either (\e -> Failed e []) id

    handleSearchError :: Either Error Suggestions -> Suggestions
    handleSearchError = handleError <<< unwrapMessage

    search :: forall e'.
              SuggestionApi
           -> Terms
           -> Aff ( ajax :: AJAX | e' ) (Either Error Suggestions)
    search api terms = attempt do
      res <- api.getSuggestions terms
      pure $ handleError $ decodeJson res.response
