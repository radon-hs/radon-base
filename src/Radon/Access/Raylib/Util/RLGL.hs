module Radon.Access.Raylib.Util.RLGL where

import Foreign.C (CFloat, CInt, CUInt)
import Radon.Access
  ( ArrayAccessor,
    FixedLengthArrayAccessor,
    PtrAccessor (..),
    StaticArrayAccessor,
    arrayAccessor,
    fixedLengthArrayAccessor,
    staticArrayAccessor,
  )
import Raylib.Types
  ( Color,
    RLDrawCall,
    RLDrawMode,
    RLRenderBatch,
    RLVertexBuffer,
    Vector2,
    Vector3,
    p'rlDrawCall'mode,
    p'rlDrawCall'textureId,
    p'rlDrawCall'vertexAlignment,
    p'rlDrawCall'vertexCount,
    p'rlRenderBatch'bufferCount,
    p'rlRenderBatch'currentBuffer,
    p'rlRenderBatch'currentDepth,
    p'rlRenderBatch'drawCounter,
    p'rlRenderBatch'draws,
    p'rlRenderBatch'vertexBuffers,
    p'rlVertexBuffer'colors,
    p'rlVertexBuffer'elementCount,
    p'rlVertexBuffer'indices,
    p'rlVertexBuffer'texcoords,
    p'rlVertexBuffer'vaoId,
    p'rlVertexBuffer'vboId,
    p'rlVertexBuffer'vertices,
  )

a'RLVertexBuffer'vertices :: ArrayAccessor RLVertexBuffer Vector3 CInt
a'RLVertexBuffer'vertices = arrayAccessor p'rlVertexBuffer'vertices p'rlVertexBuffer'elementCount

a'RLVertexBuffer'texcoords :: ArrayAccessor RLVertexBuffer Vector2 CInt
a'RLVertexBuffer'texcoords = arrayAccessor p'rlVertexBuffer'texcoords p'rlVertexBuffer'elementCount

a'RLVertexBuffer'colors :: ArrayAccessor RLVertexBuffer Color CInt
a'RLVertexBuffer'colors = arrayAccessor p'rlVertexBuffer'colors p'rlVertexBuffer'elementCount

a'RLVertexBuffer'indices :: ArrayAccessor RLVertexBuffer CUInt CInt
a'RLVertexBuffer'indices = arrayAccessor p'rlVertexBuffer'indices p'rlVertexBuffer'elementCount

a'RLVertexBuffer'vaoId :: PtrAccessor RLVertexBuffer CUInt
a'RLVertexBuffer'vaoId = PtrAccessor p'rlVertexBuffer'vaoId

a'RLVertexBuffer'vboId :: StaticArrayAccessor RLVertexBuffer CUInt Int
a'RLVertexBuffer'vboId = staticArrayAccessor (p'rlVertexBuffer'vboId) 4

a'RLDrawCall'mode :: PtrAccessor RLDrawCall RLDrawMode
a'RLDrawCall'mode = PtrAccessor p'rlDrawCall'mode

a'RLDrawCall'vertexCount :: PtrAccessor RLDrawCall CInt
a'RLDrawCall'vertexCount = PtrAccessor p'rlDrawCall'vertexCount

a'RLDrawCall'vertexAlignment :: PtrAccessor RLDrawCall CInt
a'RLDrawCall'vertexAlignment = PtrAccessor p'rlDrawCall'vertexAlignment

a'RLDrawCall'textureId :: PtrAccessor RLDrawCall CUInt
a'RLDrawCall'textureId = PtrAccessor p'rlDrawCall'textureId

a'RLRenderBatch'currentBuffer :: PtrAccessor RLRenderBatch CInt
a'RLRenderBatch'currentBuffer = PtrAccessor p'rlRenderBatch'currentBuffer

a'RLRenderBatch'vertexBuffers :: ArrayAccessor RLRenderBatch RLVertexBuffer CInt
a'RLRenderBatch'vertexBuffers = arrayAccessor p'rlRenderBatch'vertexBuffers p'rlRenderBatch'bufferCount

a'RLRenderBatch'draws :: FixedLengthArrayAccessor RLRenderBatch RLDrawCall Int
a'RLRenderBatch'draws = fixedLengthArrayAccessor p'rlRenderBatch'draws 256

a'RLRenderBatch'drawCounter :: PtrAccessor RLRenderBatch CInt
a'RLRenderBatch'drawCounter = PtrAccessor p'rlRenderBatch'drawCounter

a'RLRenderBatch'currentDepth :: PtrAccessor RLRenderBatch CFloat
a'RLRenderBatch'currentDepth = PtrAccessor p'rlRenderBatch'currentDepth