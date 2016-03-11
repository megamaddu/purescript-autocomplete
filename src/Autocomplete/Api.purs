module Autocomplete.Api
  ( SuggestionApi
  , mkDefaultApi
  ) where

import Autocomplete.Types (Suggestions, Terms)
import Control.Monad.Aff (Aff)
import Data.Argonaut.Decode (decodeJson)
import Data.Either (Either)
import Global (encodeURIComponent)
import Network.HTTP.Affjax (AJAX, get)
import Prelude

type SuggestionApi = { getSuggestions :: forall e.
                                         Terms
                                      -> Aff ( ajax :: AJAX
                                             | e
                                             ) (Either String Suggestions)
                     }

mkDefaultApi :: String -> SuggestionApi
mkDefaultApi baseUri = { getSuggestions }
  where
    getSuggestions :: forall e. Terms -> Aff ( ajax :: AJAX
                                             | e
                                             ) (Either String Suggestions)
    getSuggestions terms = do
      res <- get $ baseUri <> encodeURIComponent terms
      pure $ decodeJson res.response
