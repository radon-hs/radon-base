module Radon.Access.Defaults.Maybe where

import Foreign (plusPtr)
import Radon.Access (ReferenceAccessor (ReferenceAccessor))

-- | WARNING: will fail on `Nothing`
a'Maybe'value :: ReferenceAccessor (Maybe a) a
a'Maybe'value = ReferenceAccessor (`plusPtr` 0)
