module Autocomplete
  ( mkSuggester
  , mkSuggester'
  , FetchFn
  , SuggesterSettings
  , SuggesterInstance
  ) where

import Prelude

import Autocomplete.Store (SuggesterAction(..), SuggesterState(SuggesterState), getSuggestionResults, hasSuggestionResults, updateSuggestions)
import Autocomplete.Types (SuggestionResults, Suggestions(..), Terms)
import Control.Alt (alt, (<|>))
import Data.Either (Either(..), either)
import Data.Foldable (for_, traverse_)
import Data.List (List(..), head, take, takeEnd)
import Data.Map (singleton)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (over)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(Tuple), uncurry)
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_, runAff_, try)
import Effect.Class (liftEffect)
import Effect.Console (error, errorShow, log, warn)
import Effect.Exception (message)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Event
import FRP.Event.Time
-- import Signal (constant, dropRepeats, foldp, map2, merge, runSignal, unwrap, (<~), (~>))
-- import Signal.Channel (Channel, send, subscribe, channel)
-- import Signal.Time (debounce, millisecond)
import Unsafe.Coerce (unsafeCoerce)

-- 1. fix autocomplete as is
-- 2. change autocomlpete impl
-- 3. no cache

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

-- | Create a suggester by providing a fetch function.
mkSuggester :: forall a . Eq a => FetchFn a -> Effect (SuggesterInstance a)
mkSuggester fetch = mkSuggester'
  { fetch
  , inputDebounce: Nothing
  }

-- | Create a suggester by providing a fetch function and input debounce duration.
mkSuggester' :: forall a. Eq a => SuggesterSettings a -> Effect (SuggesterInstance a)
mkSuggester' settings = do
  { push: pushTerm, event: terms } <- create
  let output :: Event (Suggestions a)
      output = finish <$> fold act actions { latest: "", state: SuggesterState mempty }

      actions :: Event (SuggesterAction a)
      actions = keepLatest (map runSearch (debounce (Milliseconds 100.0) terms))

      act :: SuggesterAction a
          -> { latest :: Terms
             , state :: SuggesterState a
             }
          -> { latest :: Terms
             , state :: SuggesterState a
             }
      act NoAction = identity
      act (PushTerm tms) = \{ state } ->
        { latest: tms
        , state
        }
      act (AddResults (Tuple tms results)) = \{ latest, state } ->
        { latest
        , state: over SuggesterState (Map.insert tms results) state
        }

      finish :: { latest :: Terms
                , state :: SuggesterState a
                }
             -> Suggestions a
      finish { latest, state } = getSuggestionResults (pure latest) state

      runSearch :: Terms -> Event (SuggesterAction a)
      runSearch t = pure (PushTerm t) <|> pure (AddResults (Tuple t (Loading []))) <|> makeEvent \k -> do
          runAff_ (either errorShow (handleResults >>> Tuple t >>> AddResults >>> k)) (settings.fetch t)
          pure (pure unit)
        where
          handleResults e = either (\msg -> Failed msg []) Ready e

      dropRepeats = identity :: Event ~> Event
  pure { send: pushTerm
       , subscribe: subscribe output >>> void
       }
  -- termChan <- channel ""
  -- actionsChan <- channel NoAction
  -- let
  --   termsHistory = foldp (\x xs -> Cons x $ take 100 xs) Nil
  --     $ dropRepeats
  --     $ maybe identity (case _ of Milliseconds s -> debounce s) settings.inputDebounce
  --     $ subscribe termChan
  --   stores = foldp updateSuggestions initialStore (spy "actions" <~ (subscribe actionsChan))
  --   currentTermsAndStores = map2 Tuple termsHistory stores

  -- -- | Changes to the store trigger a fetch if the new current term does not have results
  -- runSignal $ currentTermsAndStores ~> \(Tuple t s) -> do
  --   runSearch settings.fetch actionsChan (fromMaybe "" $ head t) s

  -- let output = dropRepeats $ debounce 0.0 $ uncurry getSuggestionResults <~ currentTermsAndStores
  -- pure { send: send termChan
  --      , subscribe: \cb -> runSignal (output ~> cb)
  --      }
  -- where
    -- initialStore = SuggesterState mempty

-- | Internal function for running the fetch function when needed.
-- runSearch
--   :: forall a
--    . FetchFn a
--   -> Channel (SuggesterAction a)
--   -> Terms
--   -> SuggesterState a
--   -> Effect Unit
-- runSearch fetch chan terms state = do
--   if hasSuggestionResults terms state
--     then pure unit
--     else runAff_ (either errorShow pure) do
--       isAsync <- alt (const false <$> ((liftEffect <<< handleResults) =<< fetch terms))
--                      (const true <$> (delay $ Milliseconds 0.0))
--       when isAsync do
--         liftEffect $ send chan $ AddResults $ Tuple terms $ Loading []
--   where
--     handleResults e = send chan $ AddResults $ Tuple terms results
--       where results = spy ("search results " <> show terms) $ either (\msg -> Failed msg []) Ready e
