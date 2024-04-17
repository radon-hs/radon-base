{-# LANGUAGE TemplateHaskell #-}

module Radon.Access.Raylib.Core where

import Foreign.C (CFloat, CInt, CString, CUChar, CUInt)
import Radon.Access (ArrayAccessor, PtrAccessor (PtrAccessor), StaticArrayAccessor, arrayAccessor, staticArrayAccessor)
import Raylib.Types
  ( AutomationEvent,
    AutomationEventList,
    Color,
    FilePathList,
    Matrix,
    Rectangle,
    Vector2,
    Vector3,
    Vector4,
    VrDeviceInfo,
    VrStereoConfig,
    p'automationEvent'frame,
    p'automationEvent'params,
    p'automationEvent'type,
    p'automationEventList'capacity,
    p'automationEventList'count,
    p'automationEventList'events,
    p'color'a,
    p'color'b,
    p'color'g,
    p'color'r,
    p'filePathList'capacity,
    p'filePathList'count,
    p'filePathList'paths,
    p'matrix'm0,
    p'matrix'm1,
    p'matrix'm10,
    p'matrix'm11,
    p'matrix'm12,
    p'matrix'm13,
    p'matrix'm14,
    p'matrix'm15,
    p'matrix'm2,
    p'matrix'm3,
    p'matrix'm4,
    p'matrix'm5,
    p'matrix'm6,
    p'matrix'm7,
    p'matrix'm8,
    p'matrix'm9,
    p'rectangle'height,
    p'rectangle'width,
    p'rectangle'x,
    p'rectangle'y,
    p'vector2'x,
    p'vector2'y,
    p'vector3'x,
    p'vector3'y,
    p'vector3'z,
    p'vector4'w,
    p'vector4'x,
    p'vector4'y,
    p'vector4'z,
    p'vrDeviceInfo'chromaAbCorrection,
    p'vrDeviceInfo'eyeToScreenDistance,
    p'vrDeviceInfo'hResolution,
    p'vrDeviceInfo'hScreenSize,
    p'vrDeviceInfo'interpupillaryDistance,
    p'vrDeviceInfo'lensDistortionValues,
    p'vrDeviceInfo'lensSeparationDistance,
    p'vrDeviceInfo'vResolution,
    p'vrDeviceInfo'vScreenSize,
    p'vrStereoConfig'leftLensCenter,
    p'vrStereoConfig'leftScreenCenter,
    p'vrStereoConfig'projection,
    p'vrStereoConfig'rightLensCenter,
    p'vrStereoConfig'rightScreenCenter,
    p'vrStereoConfig'scale,
    p'vrStereoConfig'scaleIn,
    p'vrStereoConfig'viewOffset,
  )

a'Vector2'x :: PtrAccessor Vector2 CFloat
a'Vector2'x = PtrAccessor p'vector2'x

a'Vector2'y :: PtrAccessor Vector2 CFloat
a'Vector2'y = PtrAccessor p'vector2'y

a'Vector3'x :: PtrAccessor Vector3 CFloat
a'Vector3'x = PtrAccessor p'vector3'x

a'Vector3'y :: PtrAccessor Vector3 CFloat
a'Vector3'y = PtrAccessor p'vector3'y

a'Vector3'z :: PtrAccessor Vector3 CFloat
a'Vector3'z = PtrAccessor p'vector3'z

a'Vector4'x :: PtrAccessor Vector4 CFloat
a'Vector4'x = PtrAccessor p'vector4'x

a'Vector4'y :: PtrAccessor Vector4 CFloat
a'Vector4'y = PtrAccessor p'vector4'y

a'Vector4'z :: PtrAccessor Vector4 CFloat
a'Vector4'z = PtrAccessor p'vector4'z

a'Vector4'w :: PtrAccessor Vector4 CFloat
a'Vector4'w = PtrAccessor p'vector4'w

a'Matrix'm0 :: PtrAccessor Matrix CFloat
a'Matrix'm0 = PtrAccessor p'matrix'm0

a'Matrix'm4 :: PtrAccessor Matrix CFloat
a'Matrix'm4 = PtrAccessor p'matrix'm4

a'Matrix'm8 :: PtrAccessor Matrix CFloat
a'Matrix'm8 = PtrAccessor p'matrix'm8

a'Matrix'm12 :: PtrAccessor Matrix CFloat
a'Matrix'm12 = PtrAccessor p'matrix'm12

a'Matrix'm1 :: PtrAccessor Matrix CFloat
a'Matrix'm1 = PtrAccessor p'matrix'm1

a'Matrix'm5 :: PtrAccessor Matrix CFloat
a'Matrix'm5 = PtrAccessor p'matrix'm5

a'Matrix'm9 :: PtrAccessor Matrix CFloat
a'Matrix'm9 = PtrAccessor p'matrix'm9

a'Matrix'm13 :: PtrAccessor Matrix CFloat
a'Matrix'm13 = PtrAccessor p'matrix'm13

a'Matrix'm2 :: PtrAccessor Matrix CFloat
a'Matrix'm2 = PtrAccessor p'matrix'm2

a'Matrix'm6 :: PtrAccessor Matrix CFloat
a'Matrix'm6 = PtrAccessor p'matrix'm6

a'Matrix'm10 :: PtrAccessor Matrix CFloat
a'Matrix'm10 = PtrAccessor p'matrix'm10

a'Matrix'm14 :: PtrAccessor Matrix CFloat
a'Matrix'm14 = PtrAccessor p'matrix'm14

a'Matrix'm3 :: PtrAccessor Matrix CFloat
a'Matrix'm3 = PtrAccessor p'matrix'm3

a'Matrix'm7 :: PtrAccessor Matrix CFloat
a'Matrix'm7 = PtrAccessor p'matrix'm7

a'Matrix'm11 :: PtrAccessor Matrix CFloat
a'Matrix'm11 = PtrAccessor p'matrix'm11

a'Matrix'm15 :: PtrAccessor Matrix CFloat
a'Matrix'm15 = PtrAccessor p'matrix'm15

a'Color'r :: PtrAccessor Color CUChar
a'Color'r = PtrAccessor p'color'r

a'Color'g :: PtrAccessor Color CUChar
a'Color'g = PtrAccessor p'color'g

a'Color'b :: PtrAccessor Color CUChar
a'Color'b = PtrAccessor p'color'b

a'Color'a :: PtrAccessor Color CUChar
a'Color'a = PtrAccessor p'color'a

a'Rectangle'x :: PtrAccessor Rectangle CFloat
a'Rectangle'x = PtrAccessor p'rectangle'x

a'Rectangle'y :: PtrAccessor Rectangle CFloat
a'Rectangle'y = PtrAccessor p'rectangle'y

a'Rectangle'width :: PtrAccessor Rectangle CFloat
a'Rectangle'width = PtrAccessor p'rectangle'width

a'Rectangle'height :: PtrAccessor Rectangle CFloat
a'Rectangle'height = PtrAccessor p'rectangle'height

a'VrDeviceInfo'hResolution :: PtrAccessor VrDeviceInfo CInt
a'VrDeviceInfo'hResolution = PtrAccessor p'vrDeviceInfo'hResolution

a'VrDeviceInfo'vResolution :: PtrAccessor VrDeviceInfo CInt
a'VrDeviceInfo'vResolution = PtrAccessor p'vrDeviceInfo'vResolution

a'VrDeviceInfo'hScreenSize :: PtrAccessor VrDeviceInfo CFloat
a'VrDeviceInfo'hScreenSize = PtrAccessor p'vrDeviceInfo'hScreenSize

a'VrDeviceInfo'vScreenSize :: PtrAccessor VrDeviceInfo CFloat
a'VrDeviceInfo'vScreenSize = PtrAccessor p'vrDeviceInfo'vScreenSize

a'VrDeviceInfo'eyeToScreenDistance :: PtrAccessor VrDeviceInfo CFloat
a'VrDeviceInfo'eyeToScreenDistance = PtrAccessor p'vrDeviceInfo'eyeToScreenDistance

a'VrDeviceInfo'lensSeparationDistance :: PtrAccessor VrDeviceInfo CFloat
a'VrDeviceInfo'lensSeparationDistance = PtrAccessor p'vrDeviceInfo'lensSeparationDistance

a'VrDeviceInfo'interpupillaryDistance :: PtrAccessor VrDeviceInfo CFloat
a'VrDeviceInfo'interpupillaryDistance = PtrAccessor p'vrDeviceInfo'interpupillaryDistance

a'VrDeviceInfo'lensDistortionValues :: StaticArrayAccessor VrDeviceInfo CFloat Int
a'VrDeviceInfo'lensDistortionValues = staticArrayAccessor (p'vrDeviceInfo'lensDistortionValues) 4

a'VrDeviceInfo'chromaAbCorrection :: StaticArrayAccessor VrDeviceInfo CFloat Int
a'VrDeviceInfo'chromaAbCorrection = staticArrayAccessor (p'vrDeviceInfo'chromaAbCorrection) 4

a'VrStereoConfig'projection :: StaticArrayAccessor VrStereoConfig Matrix Int
a'VrStereoConfig'projection = staticArrayAccessor (p'vrStereoConfig'projection) 2

a'VrStereoConfig'viewOffset :: StaticArrayAccessor VrStereoConfig Matrix Int
a'VrStereoConfig'viewOffset = staticArrayAccessor (p'vrStereoConfig'viewOffset) 2

a'VrStereoConfig'leftLensCenter :: StaticArrayAccessor VrStereoConfig CFloat Int
a'VrStereoConfig'leftLensCenter = staticArrayAccessor (p'vrStereoConfig'leftLensCenter) 2

a'VrStereoConfig'rightLensCenter :: StaticArrayAccessor VrStereoConfig CFloat Int
a'VrStereoConfig'rightLensCenter = staticArrayAccessor (p'vrStereoConfig'rightLensCenter) 2

a'VrStereoConfig'leftScreenCenter :: StaticArrayAccessor VrStereoConfig CFloat Int
a'VrStereoConfig'leftScreenCenter = staticArrayAccessor (p'vrStereoConfig'leftScreenCenter) 2

a'VrStereoConfig'rightScreenCenter :: StaticArrayAccessor VrStereoConfig CFloat Int
a'VrStereoConfig'rightScreenCenter = staticArrayAccessor (p'vrStereoConfig'rightScreenCenter) 2

a'VrStereoConfig'scale :: StaticArrayAccessor VrStereoConfig CFloat Int
a'VrStereoConfig'scale = staticArrayAccessor (p'vrStereoConfig'scale) 2

a'VrStereoConfig'scaleIn :: StaticArrayAccessor VrStereoConfig CFloat Int
a'VrStereoConfig'scaleIn = staticArrayAccessor (p'vrStereoConfig'scaleIn) 2

a'FilePathList'capacity :: PtrAccessor FilePathList CUInt
a'FilePathList'capacity = PtrAccessor p'filePathList'capacity

a'FilePathList'paths :: ArrayAccessor FilePathList CString CUInt
a'FilePathList'paths = arrayAccessor p'filePathList'paths p'filePathList'count

a'AutomationEvent'frame :: PtrAccessor AutomationEvent CUInt
a'AutomationEvent'frame = PtrAccessor p'automationEvent'frame

a'AutomationEvent'type :: PtrAccessor AutomationEvent CUInt
a'AutomationEvent'type = PtrAccessor p'automationEvent'type

a'AutomationEvent'params :: StaticArrayAccessor AutomationEvent CInt Int
a'AutomationEvent'params = staticArrayAccessor (p'automationEvent'params) 4

a'AutomationEventList'capacity :: PtrAccessor AutomationEventList CUInt
a'AutomationEventList'capacity = PtrAccessor p'automationEventList'capacity

a'AutomationEventList'events :: ArrayAccessor AutomationEventList AutomationEvent CUInt
a'AutomationEventList'events = arrayAccessor p'automationEventList'events p'automationEventList'count
