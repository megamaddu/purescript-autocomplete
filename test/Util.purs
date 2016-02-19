module Test.Util where

import Prelude (Unit, ($), unit, pure)
import Control.Monad.Aff (Aff, later')

wait :: forall e. Int -> Aff e Unit
wait t = later' t $ pure unit
