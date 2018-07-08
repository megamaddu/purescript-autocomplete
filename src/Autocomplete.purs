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
import Control.Alt (alt)
import Data.Either (Either(..), either)
import Data.Foldable (for_, traverse_)
import Data.Map (singleton)
import Data.Maybe (Maybe(..), maybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_, runAff_, try)
import Effect.Class (liftEffect)
import Effect.Console (error, errorShow, warn)
import Effect.Exception (message)
import Signal ((~>), (<~), runSignal, dropRepeats, unwrap, foldp, merge)
import Signal.Channel (Channel, send, subscribe, channel)
import Signal.Time (debounce, millisecond)

type FetchFn a = Terms -> Aff (Either String (Array a))

type SuggesterSettings a =
  { fetch :: FetchFn a
  , inputDebounce :: Maybe Milliseconds
  }

-- | A suggester instance has a `send` function for providing new terms
-- | and a subscribe function for listening for new suggestions.
-- | The subscribe function will always be called with the most up to date
-- | suggestions for the latest terms sent, even if a previous search is
-- | slow to complete.  Subscribers immediately receive the most recent
-- | result set.
type SuggesterInstance a =
  { send :: Terms -> Effect Unit
  , subscribe :: (Suggestions a -> Effect Unit) -> Effect Unit
  }

-- | Create a suggester with the default API backend: Affjax.get & decodeJson,
-- | no input debounce, and no input transformations.
mkSuggester :: forall a . Eq a => FetchFn a -> Effect (SuggesterInstance a)
mkSuggester fetch = mkSuggester'
  { fetch
  , inputDebounce: Nothing
  }

-- | Create a suggester with an alternate API backend.
mkSuggester' :: forall a. Eq a => SuggesterSettings a -> Effect (SuggesterInstance a)
mkSuggester' settings = do
  termChan <- channel mempty
  searchResChan <- channel mempty
  let
    terms = SetTerms <~ dropRepeats
      ( maybe identity (case _ of Milliseconds s -> debounce s) settings.inputDebounce
          $ subscribe termChan
      )
    searchRes = AddResults <~ subscribe searchResChan
    suggesterActions = merge terms searchRes
    stores = foldp updateSuggestions initialStore suggesterActions

  -- | Changes to the store trigger a fetch if the new current term does not have results
  runSignal $ stores ~> runSearch settings.fetch searchResChan

  let output = dropRepeats $ getSuggestionResults <~ stores
  pure { send: send termChan
       , subscribe: \cb -> runSignal (output ~> cb)
       }
  where
    initialStore = SuggesterState
      { currentTerms: mempty
      , currentResults: mempty
      , termsHistory: mempty
      , store: mempty
      }

-- | Internal function for running the fetch function when needed.
runSearch
  :: forall a
   . FetchFn a
  -> Channel (SuggestionResults a)
  -> (SuggesterState a)
  -> Effect Unit
runSearch fetch chan st@(SuggesterState store) = do
  if hasSuggestionResults st
    then pure unit
    else runAff_ (either errorShow pure) do
      isAsync <- alt (const false <$> ((liftEffect <<< handleResults) =<< fetch terms))
                     (const true <$> (delay $ Milliseconds 0.0))
      when isAsync do
        liftEffect $ send chan $ Tuple terms $ Loading []

  where
    terms = store.currentTerms
    handleResults e = send chan $ Tuple terms results
      where results = either (\msg -> Failed msg []) Ready e

