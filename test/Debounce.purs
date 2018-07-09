module Test.Debounce where

import Prelude

import Autocomplete (FetchFn, mkSuggester')
import Autocomplete.Types (Suggestions(Ready, Loading, Failed))
import Data.Array (snoc)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse, traverse_)
import Effect.Aff (delay)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
