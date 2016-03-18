module Autocomplete.Types where

import Data.Argonaut.Combinators ((.?))
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Monoid (class Monoid)
import Data.Tuple (Tuple)
import Prelude

-- | Any string.  Terms are normalized for searching. Ex:
-- |
-- | "chevron scarves" will become "chevron-scarves" when passed to the API
type Terms = String

-- | A single suggestion row.  Phrase is the text which matched the search.
-- | Hits represents relevancy (descending).
newtype Suggestion = Suggestion { phrase :: String
                                , hits :: Number }

-- | Suggestions are represented as a state and a collection of the
-- | currently most relevant suggestions.  Ex:
-- | ```
-- | Ready []
-- | Loading []
-- | Ready [a, b, c]
-- | Loading [a, b, c] <- continue displaying these results until better ones are available
-- | Ready [d, e, f]
-- | ```
data Suggestions
  = Loading (Array Suggestion)
  | Failed String (Array Suggestion)
  | Ready (Array Suggestion)

type SuggestionResults = Tuple Terms Suggestions

instance eqSuggestion :: Eq Suggestion where
  eq (Suggestion x) (Suggestion y) = x.phrase == y.phrase && x.hits == y.hits

instance eqSuggestions :: Eq Suggestions where
  eq (Loading a)  (Loading b)   = a == b
  eq (Failed e a) (Failed e' b) = e == e' && a == b
  eq (Ready a)    (Ready b)     = a == b
  eq _            _             = false

instance showSuggestion :: Show Suggestion where
  show (Suggestion x) = "{phrase: '" ++ x.phrase
                   ++ "', hits: "    ++ show x.hits ++ "}"

instance showSuggestions :: Show Suggestions where
  show (Loading x)  = "Loading" ++ show x
  show (Failed e x) = "Failed(" ++ e ++ ")--" ++ show x
  show (Ready x)    = "Ready" ++ show x

instance semigroupSuggestions :: Semigroup Suggestions where
  append _ b = b

instance monoidSuggestions :: Monoid Suggestions where
  mempty = Ready []

instance decodeJsonSuggestion :: DecodeJson Suggestion where
  decodeJson json = do
    obj <- decodeJson json
    phrase <- obj .? "phrase"
    hits <- obj .? "weight"
    pure $ Suggestion { phrase, hits }

instance decodeJsonSuggestions :: DecodeJson Suggestions where
  decodeJson json = do
    arr <- decodeJson json
    pure $ Ready arr
