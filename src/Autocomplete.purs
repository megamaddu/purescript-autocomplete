module Autocomplete
  ( mkSuggester
  , mkSuggester'
  , FetchFn
  , SuggesterSettings
  , SuggesterInstance
  ) where

import Prelude

import Autocomplete.Store (SuggesterAction(..), SuggesterState(SuggesterState), getNextBestResults, getSuggestionResults, hasSuggestionResults, updateSuggestions)
import Autocomplete.Types (SuggestionResults, Suggestions(..), Terms)
import Control.Alt (alt, (<|>))
import Data.Either (Either(..), either)
import Data.Foldable (for_, traverse_)
import Data.List (List(..), head, take, takeEnd)
import Data.Map (singleton)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(Tuple), uncurry)
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_, runAff_, try)
import Effect.Class (liftEffect)
import Effect.Console (error, errorShow, warn)
import Effect.Exception (message)
import Signal (constant, dropRepeats, foldp, map2, merge, runSignal, unwrap, (<~), (~>))
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
  actionsChan <- channel NoAction
  let
    terms = foldp (\x xs -> Cons x $ take 100 xs) Nil
      $ dropRepeats
      $ maybe identity (case _ of Milliseconds s -> debounce s) settings.inputDebounce
      $ map (spy "terms")
      $ subscribe termChan
    stores = foldp updateSuggestions initialStore (subscribe actionsChan)
    outputs = constant (Tuple mempty (SuggesterState mempty)) -- map2 Tuple terms stores

  -- | Changes to the store trigger a fetch if the new current term does not have results
  -- runSignal $ outputs ~> \(Tuple t s) -> runSearch settings.fetch actionsChan (fromMaybe "" $ head t) s

  let output = spy "results" <~ dropRepeats (debounce 0.0 (uncurry getNextBestResults <~ outputs))
  pure { send: send termChan
       , subscribe: \cb -> runSignal (output ~> cb)
       }
  where
    initialStore = SuggesterState mempty

-- | Internal function for running the fetch function when needed.
runSearch
  :: forall a
   . FetchFn a
  -> Channel (SuggesterAction a)
  -> Terms
  -> SuggesterState a
  -> Effect Unit
runSearch fetch chan terms state = do
  if spy "runSearch:hasSuggestionResults" (hasSuggestionResults (spy "runSearch:terms" terms) (spy "runSearch:state" state))
    then pure unit
    else runAff_ (either errorShow pure) do
      isAsync <- alt (const false <$> ((liftEffect <<< handleResults) =<< fetch terms))
                     (const true <$> (delay $ Milliseconds 0.0))
      when (spy "runSearch:isAsync" isAsync) do
        -- spy "setting loading state" $ liftEffect $ send chan $ AddResults $ Tuple terms $ Loading []
        spy "runSearch:setting loading state" $ pure unit

  where
    handleResults e = send chan $ AddResults $ Tuple terms results
      where results = either (\msg -> Failed msg []) Ready e

