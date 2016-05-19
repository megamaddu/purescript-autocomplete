module Test.Main where

import Prelude
import Test.Autocomplete as Autocomplete
import Test.Autocomplete.Store as Store
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Network.HTTP.Affjax (AJAX)
import Signal.Channel (CHANNEL)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main ::
  Eff
    ( console :: CONSOLE
    , testOutput :: TESTOUTPUT
    , channel :: CHANNEL
    , ajax :: AJAX
    , err :: EXCEPTION
    , ref :: REF
    ) Unit
main = runTest do
  Store.runTests
  Autocomplete.runTests
