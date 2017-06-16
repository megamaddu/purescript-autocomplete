module Test.Util where

import Control.Monad.Aff (Aff, delay)
import Data.Function (($))
import Data.Int (toNumber)
import Data.Time.Duration (Milliseconds(..))
import Prelude (Unit)

wait :: forall e. Int -> Aff e Unit
wait t = delay $ Milliseconds $ toNumber t
