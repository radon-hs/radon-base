module Radon.Access.Raylib.Core.Text where

import Foreign.C (CInt)
import Radon.Access
  ( ArrayAccessor,
    PtrAccessor (..),
    arrayAccessor,
  )
import Raylib.Types
  ( Font,
    GlyphInfo,
    Image,
    Rectangle,
    Texture,
    p'font'baseSize,
    p'font'glyphCount,
    p'font'glyphPadding,
    p'font'glyphs,
    p'font'recs,
    p'font'texture,
    p'glyphInfo'advanceX,
    p'glyphInfo'image,
    p'glyphInfo'offsetX,
    p'glyphInfo'offsetY,
    p'glyphInfo'value,
  )

a'GlyphInfo'value :: PtrAccessor GlyphInfo CInt
a'GlyphInfo'value = PtrAccessor p'glyphInfo'value

a'GlyphInfo'offsetX :: PtrAccessor GlyphInfo CInt
a'GlyphInfo'offsetX = PtrAccessor p'glyphInfo'offsetX

a'GlyphInfo'offsetY :: PtrAccessor GlyphInfo CInt
a'GlyphInfo'offsetY = PtrAccessor p'glyphInfo'offsetY

a'GlyphInfo'advanceX :: PtrAccessor GlyphInfo CInt
a'GlyphInfo'advanceX = PtrAccessor p'glyphInfo'advanceX

a'GlyphInfo'image :: PtrAccessor GlyphInfo Image
a'GlyphInfo'image = PtrAccessor p'glyphInfo'image

a'Font'baseSize :: PtrAccessor Font CInt
a'Font'baseSize = PtrAccessor p'font'baseSize

a'Font'glyphPadding :: PtrAccessor Font CInt
a'Font'glyphPadding = PtrAccessor p'font'glyphPadding

a'Font'texture :: PtrAccessor Font Texture
a'Font'texture = PtrAccessor p'font'texture

a'Font'recs :: ArrayAccessor Font Rectangle CInt
a'Font'recs = arrayAccessor p'font'recs p'font'glyphCount

a'Font'glyphs :: ArrayAccessor Font GlyphInfo CInt
a'Font'glyphs = arrayAccessor p'font'glyphs p'font'glyphCount
