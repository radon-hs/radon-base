module Radon.Access.Defaults.String where

import Foreign (castPtr)
import Foreign.C (CString)
import Radon.Access (PtrAccessor (PtrAccessor))

a'CString'toString :: PtrAccessor CString String
a'CString'toString = PtrAccessor castPtr