module Autocomplete.Types where

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Monoid (class Monoid)
import Data.Tuple (Tuple)
import Prelude

-- | Any string.  Terms are normalized for searching. Ex:
-- |
-- | "chevron scarves" will become "chevron-scarves" when passed to the API
type Terms = String

-- | Suggestions are represented as a state and a collection of the
-- | currently most relevant suggestions.  Ex:
-- | ```
-- | Ready []
-- | Loading []
-- | Ready [a, b, c]
-- | Loading [a, b, c] <- continue displaying these results until better ones are available
-- | Ready [d, e, f]
-- | ```
data Suggestions a
  = Loading (Array a)
  | Failed String (Array a)
  | Ready (Array a)

type SuggestionResults a = Tuple Terms (Suggestions a)

derive instance eqSuggestions :: Eq a => Eq (Suggestions a)

instance showSuggestions :: Show a => Show (Suggestions a) where
  show (Loading x)  = "Loading" ++ show x
  show (Failed e x) = "Failed(" ++ e ++ ")--" ++ show x
  show (Ready x)    = "Ready" ++ show x

instance semigroupSuggestions :: Semigroup (Suggestions a) where
  append _ b = b

instance monoidSuggestions :: Monoid (Suggestions a) where
  mempty = Ready []

instance decodeJsonSuggestions :: DecodeJson a => DecodeJson (Suggestions a) where
  decodeJson json = do
    arr <- decodeJson json
    pure $ Ready arr
