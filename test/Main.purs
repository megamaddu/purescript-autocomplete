module Test.Main where

import Prelude

import Effect (Effect)
import Test.Autocomplete as Autocomplete
import Test.Autocomplete.Store as Store
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  -- Store.runTests
  Autocomplete.runTests
