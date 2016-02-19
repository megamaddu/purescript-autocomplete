module Autocomplete.Api
  ( SuggestionApi
  , mkDefaultApi
  ) where

import Autocomplete.Types (Terms)
import Data.Argonaut.Core (Json)
import Global (encodeURIComponent)
import Network.HTTP.Affjax (Affjax, get)
import Prelude (($), (<>))

type SuggestionApi = { getSuggestions :: forall e. Terms -> Affjax e Json }

mkDefaultApi :: String -> SuggestionApi
mkDefaultApi baseUri = { getSuggestions: \terms -> get $ baseUri
                                                      <> encodeURIComponent terms }
