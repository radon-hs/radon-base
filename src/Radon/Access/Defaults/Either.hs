module Radon.Access.Defaults.Either where

import Radon.Access.Class (RStorable (..))
import Radon.Access (PtrAccessor (PtrAccessor))
import Foreign (plusPtr)

a'Either :: PtrAccessor (Either a b) Bool
a'Either = PtrAccessor (`plusPtr` 0)

-- | WARNING: will fail or return garbage value on `Right`
a'Either'left :: PtrAccessor (Either a b) a
a'Either'left = PtrAccessor (`plusPtr` rsizeOf True)

-- | WARNING: will fail or return garbage value on `Left`
a'Either'right :: PtrAccessor (Either a b) b
a'Either'right = PtrAccessor (`plusPtr` rsizeOf True)
