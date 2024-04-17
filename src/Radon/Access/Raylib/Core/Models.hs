module Radon.Access.Raylib.Core.Models where

import Foreign (Ptr, castPtr)
import Foreign.C
  ( CBool,
    CChar,
    CFloat,
    CInt,
    CUChar,
    CUInt,
    CUShort,
  )
import Radon.Access
  ( ArrayAccessor,
    FixedLengthArrayAccessor,
    MaybeArrayAccessor,
    PtrAccessor (..),
    StaticArrayAccessor,
    arrayAccessor,
    fixedLengthArrayAccessor,
    maybeArrayAccessor,
    staticArrayAccessor,
  )
import Raylib.Types
  ( BoneInfo,
    BoundingBox,
    Color,
    Material,
    MaterialMap,
    Matrix,
    Mesh,
    Model,
    ModelAnimation,
    Quaternion,
    Ray,
    RayCollision,
    Shader,
    Texture,
    Transform,
    Vector2,
    Vector3,
    Vector4,
    p'boneInfo'name,
    p'boneInfo'parent,
    p'boundingBox'max,
    p'boundingBox'min,
    p'material'maps,
    p'material'params,
    p'material'shader,
    p'materialMap'color,
    p'materialMap'texture,
    p'materialMap'value,
    p'mesh'animNormals,
    p'mesh'animVertices,
    p'mesh'boneIds,
    p'mesh'boneWeights,
    p'mesh'colors,
    p'mesh'indices,
    p'mesh'normals,
    p'mesh'tangents,
    p'mesh'texcoords,
    p'mesh'texcoords2,
    p'mesh'triangleCount,
    p'mesh'vaoId,
    p'mesh'vboId,
    p'mesh'vertexCount,
    p'mesh'vertices,
    p'model'bindPose,
    p'model'boneCount,
    p'model'bones,
    p'model'materialCount,
    p'model'materials,
    p'model'meshCount,
    p'model'meshMaterial,
    p'model'meshes,
    p'model'transform,
    p'modelAnimation'boneCount,
    p'modelAnimation'bones,
    p'modelAnimation'frameCount,
    p'modelAnimation'framePoses,
    p'modelAnimation'name,
    p'ray'direction,
    p'ray'position,
    p'rayCollision'distance,
    p'rayCollision'hit,
    p'rayCollision'normal,
    p'rayCollision'point,
    p'shader'id,
    p'shader'locs,
    p'transform'rotation,
    p'transform'scale,
    p'transform'translation,
  )

a'Mesh'triangleCount :: PtrAccessor Mesh CInt
a'Mesh'triangleCount = PtrAccessor p'mesh'triangleCount

a'Mesh'vertices :: ArrayAccessor Mesh Vector3 CInt
a'Mesh'vertices = arrayAccessor p'mesh'vertices p'mesh'vertexCount

a'Mesh'texcoords :: ArrayAccessor Mesh Vector2 CInt
a'Mesh'texcoords = arrayAccessor p'mesh'texcoords p'mesh'vertexCount

a'Mesh'texcoords2 :: ArrayAccessor Mesh Vector2 CInt
a'Mesh'texcoords2 = arrayAccessor p'mesh'texcoords2 p'mesh'vertexCount

a'Mesh'normals :: ArrayAccessor Mesh Vector3 CInt
a'Mesh'normals = arrayAccessor p'mesh'normals p'mesh'vertexCount

a'Mesh'tangents :: ArrayAccessor Mesh Vector4 CInt
a'Mesh'tangents = arrayAccessor p'mesh'tangents p'mesh'vertexCount

a'Mesh'colors :: ArrayAccessor Mesh Color CInt
a'Mesh'colors = arrayAccessor p'mesh'colors p'mesh'vertexCount

a'Mesh'indices :: ArrayAccessor Mesh CUShort CInt
a'Mesh'indices = arrayAccessor p'mesh'indices p'mesh'vertexCount

a'Mesh'animVertices :: ArrayAccessor Mesh Vector3 CInt
a'Mesh'animVertices = arrayAccessor p'mesh'animVertices p'mesh'vertexCount

a'Mesh'animNormals :: ArrayAccessor Mesh Vector3 CInt
a'Mesh'animNormals = arrayAccessor p'mesh'animNormals p'mesh'vertexCount

a'Mesh'boneIds :: MaybeArrayAccessor Mesh (CUChar, CUChar, CUChar, CUChar) CInt
a'Mesh'boneIds = maybeArrayAccessor (castPtr . p'mesh'boneIds) p'mesh'vertexCount

a'Mesh'boneWeights :: MaybeArrayAccessor Mesh (CFloat, CFloat, CFloat, CFloat) CInt
a'Mesh'boneWeights = maybeArrayAccessor (castPtr . p'mesh'boneWeights) p'mesh'vertexCount

a'Mesh'vaoId :: PtrAccessor Mesh CUInt
a'Mesh'vaoId = PtrAccessor p'mesh'vaoId

a'Mesh'vboId :: FixedLengthArrayAccessor Mesh CUInt Int
a'Mesh'vboId = fixedLengthArrayAccessor p'mesh'vboId 7

a'Shader'id :: PtrAccessor Shader CUInt
a'Shader'id = PtrAccessor p'shader'id

a'Shader'locs :: FixedLengthArrayAccessor Shader CInt Int
a'Shader'locs = fixedLengthArrayAccessor p'shader'locs 32

a'MaterialMap'texture :: PtrAccessor MaterialMap Texture
a'MaterialMap'texture = PtrAccessor p'materialMap'texture

a'MaterialMap'color :: PtrAccessor MaterialMap Color
a'MaterialMap'color = PtrAccessor p'materialMap'color

a'MaterialMap'value :: PtrAccessor MaterialMap CFloat
a'MaterialMap'value = PtrAccessor p'materialMap'value

a'Material'shader :: PtrAccessor Material Shader
a'Material'shader = PtrAccessor p'material'shader

a'Material'maps :: FixedLengthArrayAccessor Material MaterialMap Int
a'Material'maps = fixedLengthArrayAccessor p'material'maps 12

a'Material'params :: StaticArrayAccessor Material CFloat Int
a'Material'params = staticArrayAccessor (p'material'params) 4

a'Transform'translation :: PtrAccessor Transform Vector3
a'Transform'translation = PtrAccessor p'transform'translation

a'Transform'rotation :: PtrAccessor Transform Quaternion
a'Transform'rotation = PtrAccessor p'transform'rotation

a'Transform'scale :: PtrAccessor Transform Vector3
a'Transform'scale = PtrAccessor p'transform'scale

a'BoneInfo'name :: StaticArrayAccessor BoneInfo CChar Int
a'BoneInfo'name = staticArrayAccessor p'boneInfo'name 32

a'BoneInfo'parent :: PtrAccessor BoneInfo CInt
a'BoneInfo'parent = PtrAccessor p'boneInfo'parent

a'Model'transform :: PtrAccessor Model Matrix
a'Model'transform = PtrAccessor p'model'transform

a'Model'meshes :: ArrayAccessor Model Mesh CInt
a'Model'meshes = arrayAccessor p'model'meshes p'model'meshCount

a'Model'materials :: ArrayAccessor Model Material CInt
a'Model'materials = arrayAccessor p'model'materials p'model'materialCount

a'Model'meshMaterial :: ArrayAccessor Model CInt CInt
a'Model'meshMaterial = arrayAccessor p'model'meshMaterial p'model'meshCount

a'Model'bones :: ArrayAccessor Model BoneInfo CInt
a'Model'bones = arrayAccessor p'model'bones p'model'boneCount

a'Model'bindPose :: ArrayAccessor Model Transform CInt
a'Model'bindPose = arrayAccessor p'model'bindPose p'model'boneCount

a'ModelAnimation'frameCount :: PtrAccessor ModelAnimation CInt
a'ModelAnimation'frameCount = PtrAccessor p'modelAnimation'frameCount

a'ModelAnimation'bones :: ArrayAccessor ModelAnimation BoneInfo CInt
a'ModelAnimation'bones = arrayAccessor p'modelAnimation'bones p'modelAnimation'boneCount

a'ModelAnimation'framePoses :: ArrayAccessor ModelAnimation (Ptr Transform) CInt
a'ModelAnimation'framePoses = arrayAccessor p'modelAnimation'framePoses p'modelAnimation'frameCount

a'ModelAnimation'name :: StaticArrayAccessor ModelAnimation CChar Int
a'ModelAnimation'name = staticArrayAccessor p'modelAnimation'name 32

a'Ray'position :: PtrAccessor Ray Vector3
a'Ray'position = PtrAccessor p'ray'position

a'Ray'direction :: PtrAccessor Ray Vector3
a'Ray'direction = PtrAccessor p'ray'direction

a'RayCollision'hit :: PtrAccessor RayCollision CBool
a'RayCollision'hit = PtrAccessor p'rayCollision'hit

a'RayCollision'distance :: PtrAccessor RayCollision CFloat
a'RayCollision'distance = PtrAccessor p'rayCollision'distance

a'RayCollision'point :: PtrAccessor RayCollision Vector3
a'RayCollision'point = PtrAccessor p'rayCollision'point

a'RayCollision'normal :: PtrAccessor RayCollision Vector3
a'RayCollision'normal = PtrAccessor p'rayCollision'normal

a'BoundingBox'min :: PtrAccessor BoundingBox Vector3
a'BoundingBox'min = PtrAccessor p'boundingBox'min

a'BoundingBox'max :: PtrAccessor BoundingBox Vector3
a'BoundingBox'max = PtrAccessor p'boundingBox'max
