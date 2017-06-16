module Autocomplete.Api
  ( SuggestionApi
  , mkDefaultApi
  ) where

import Autocomplete.Types (Suggestions, Terms)
import Control.Monad.Aff (Aff)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Either (Either)
import Global (encodeURIComponent)
import Network.HTTP.Affjax (AJAX, get)
import Prelude

type SuggestionApi a =
  { getSuggestions :: forall e.
      Terms
      -> Aff
          ( ajax :: AJAX
          | e
          ) (Either String (Suggestions a))
  }

mkDefaultApi :: forall a. DecodeJson a => String -> SuggestionApi a
mkDefaultApi baseUri = { getSuggestions }
  where
    getSuggestions :: forall e.
      Terms
      -> Aff
          ( ajax :: AJAX
          | e
          ) (Either String (Suggestions a))
    getSuggestions terms = do
      res <- get $ baseUri <> encodeURIComponent terms
      pure $ decodeJson res.response
