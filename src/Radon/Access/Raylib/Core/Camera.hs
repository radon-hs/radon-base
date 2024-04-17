module Radon.Access.Raylib.Core.Camera where

import Foreign.C (CFloat)
import Radon.Access (PtrAccessor (PtrAccessor))
import Raylib.Types
  ( Camera2D,
    Camera3D,
    CameraProjection,
    Vector2,
    Vector3,
    p'camera2D'offset,
    p'camera2D'rotation,
    p'camera2D'target,
    p'camera2D'zoom,
    p'camera3D'fovy,
    p'camera3D'position,
    p'camera3D'projection,
    p'camera3D'target,
    p'camera3D'up,
  )

a'Camera3D'position :: PtrAccessor Camera3D Vector3
a'Camera3D'position = PtrAccessor p'camera3D'position

a'Camera3D'target :: PtrAccessor Camera3D Vector3
a'Camera3D'target = PtrAccessor p'camera3D'target

a'Camera3D'up :: PtrAccessor Camera3D Vector3
a'Camera3D'up = PtrAccessor p'camera3D'up

a'Camera3D'fovy :: PtrAccessor Camera3D CFloat
a'Camera3D'fovy = PtrAccessor p'camera3D'fovy

a'Camera3D'projection :: PtrAccessor Camera3D CameraProjection
a'Camera3D'projection = PtrAccessor p'camera3D'projection

a'Camera2D'offset :: PtrAccessor Camera2D Vector2
a'Camera2D'offset = PtrAccessor p'camera2D'offset

a'Camera2D'target :: PtrAccessor Camera2D Vector2
a'Camera2D'target = PtrAccessor p'camera2D'target

a'Camera2D'rotation :: PtrAccessor Camera2D CFloat
a'Camera2D'rotation = PtrAccessor p'camera2D'rotation

a'Camera2D'zoom :: PtrAccessor Camera2D CFloat
a'Camera2D'zoom = PtrAccessor p'camera2D'zoom
