module Radon.Access.Raylib.Util.GUI where

import Foreign.C (CInt, CUShort)
import Radon.Access (PtrAccessor (..))
import Raylib.Types
  ( GuiStyleProp,
    p'guiStyleProp'controlId,
    p'guiStyleProp'propertyId,
    p'guiStyleProp'propertyValue,
  )

a'GuiStyleProp'controlId :: PtrAccessor GuiStyleProp CUShort
a'GuiStyleProp'controlId = PtrAccessor p'guiStyleProp'controlId

a'GuiStyleProp'propertyId :: PtrAccessor GuiStyleProp CUShort
a'GuiStyleProp'propertyId = PtrAccessor p'guiStyleProp'propertyId

a'GuiStyleProp'propertyValue :: PtrAccessor GuiStyleProp CInt
a'GuiStyleProp'propertyValue = PtrAccessor p'guiStyleProp'propertyValue
