module Radon.Access.Raylib.Core.Textures where

import Foreign (Ptr)
import Foreign.C (CInt, CUChar, CUInt)
import Radon.Access (PtrAccessor (..))
import Raylib.Types
  ( Image,
    NPatchInfo,
    NPatchLayout,
    PixelFormat,
    Rectangle,
    RenderTexture,
    Texture,
    p'image'data,
    p'image'format,
    p'image'height,
    p'image'mipmaps,
    p'image'width,
    p'nPatchInfo'bottom,
    p'nPatchInfo'layout,
    p'nPatchInfo'left,
    p'nPatchInfo'right,
    p'nPatchInfo'source,
    p'nPatchInfo'top,
    p'renderTexture'depth,
    p'renderTexture'id,
    p'renderTexture'texture,
    p'texture'format,
    p'texture'height,
    p'texture'id,
    p'texture'mipmaps,
    p'texture'width,
  )

a'Image'data :: PtrAccessor Image (Ptr CUChar)
a'Image'data = PtrAccessor p'image'data

a'Image'width :: PtrAccessor Image CInt
a'Image'width = PtrAccessor p'image'width

a'Image'height :: PtrAccessor Image CInt
a'Image'height = PtrAccessor p'image'height

a'Image'mipmaps :: PtrAccessor Image CInt
a'Image'mipmaps = PtrAccessor p'image'mipmaps

a'Image'format :: PtrAccessor Image PixelFormat
a'Image'format = PtrAccessor p'image'format

a'Texture'id :: PtrAccessor Texture CUInt
a'Texture'id = PtrAccessor p'texture'id

a'Texture'width :: PtrAccessor Texture CInt
a'Texture'width = PtrAccessor p'texture'width

a'Texture'height :: PtrAccessor Texture CInt
a'Texture'height = PtrAccessor p'texture'height

a'Texture'mipmaps :: PtrAccessor Texture CInt
a'Texture'mipmaps = PtrAccessor p'texture'mipmaps

a'Texture'format :: PtrAccessor Texture PixelFormat
a'Texture'format = PtrAccessor p'texture'format

a'RenderTexture'id :: PtrAccessor RenderTexture CUInt
a'RenderTexture'id = PtrAccessor p'renderTexture'id

a'RenderTexture'texture :: PtrAccessor RenderTexture Texture
a'RenderTexture'texture = PtrAccessor p'renderTexture'texture

a'RenderTexture'depth :: PtrAccessor RenderTexture Texture
a'RenderTexture'depth = PtrAccessor p'renderTexture'depth

a'NPatchInfo'source :: PtrAccessor NPatchInfo Rectangle
a'NPatchInfo'source = PtrAccessor p'nPatchInfo'source

a'NPatchInfo'left :: PtrAccessor NPatchInfo CInt
a'NPatchInfo'left = PtrAccessor p'nPatchInfo'left

a'NPatchInfo'top :: PtrAccessor NPatchInfo CInt
a'NPatchInfo'top = PtrAccessor p'nPatchInfo'top

a'NPatchInfo'right :: PtrAccessor NPatchInfo CInt
a'NPatchInfo'right = PtrAccessor p'nPatchInfo'right

a'NPatchInfo'bottom :: PtrAccessor NPatchInfo CInt
a'NPatchInfo'bottom = PtrAccessor p'nPatchInfo'bottom

a'NPatchInfo'layout :: PtrAccessor NPatchInfo NPatchLayout
a'NPatchInfo'layout = PtrAccessor p'nPatchInfo'layout
