-- | This code is temporary until these functions are
-- | merged into purescript-signal:
-- | https://github.com/bodil/purescript-signal/pull/38
module Util.Signal where

import Prelude (class Eq, ($), (==))
import Signal (Signal, dropRepeats, filter, sampleOn)
import Signal.Time (Time, since)

debounce :: forall a. Time -> Signal a -> Signal a
debounce t s =
  let leading = whenChangeTo false $ since t s
  in sampleOn leading s

whenEqual :: forall a. (Eq a) => a -> Signal a -> Signal a
whenEqual value input = filter ((==) value) value input

whenChangeTo :: forall a. (Eq a) => a -> Signal a -> Signal a
whenChangeTo value input = whenEqual value $ dropRepeats input
