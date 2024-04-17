module Radon.Access.Raylib.Core.Audio where

import Foreign (Ptr, castPtr)
import Foreign.C (CBool, CFloat, CShort, CUChar, CUInt)
import Radon.Access (PtrAccessor (PtrAccessor), StaticArrayAccessor, staticArrayAccessor)
import Raylib.Types
  ( AudioBufferUsage,
    AudioStream,
    C'AudioCallback,
    Music,
    MusicContextType,
    RAudioBuffer,
    RAudioProcessor,
    Sound,
    Wave,
    p'audioStream'buffer,
    p'audioStream'channels,
    p'audioStream'processor,
    p'audioStream'sampleRate,
    p'audioStream'sampleSize,
    p'music'ctxData,
    p'music'ctxType,
    p'music'frameCount,
    p'music'looping,
    p'music'stream,
    p'rAudioBuffer'callback,
    p'rAudioBuffer'converter,
    p'rAudioBuffer'data,
    p'rAudioBuffer'frameCursorPos,
    p'rAudioBuffer'framesProcessed,
    p'rAudioBuffer'isSubBufferProcessed,
    p'rAudioBuffer'looping,
    p'rAudioBuffer'next,
    p'rAudioBuffer'pan,
    p'rAudioBuffer'paused,
    p'rAudioBuffer'pitch,
    p'rAudioBuffer'playing,
    p'rAudioBuffer'prev,
    p'rAudioBuffer'processor,
    p'rAudioBuffer'sizeInFrames,
    p'rAudioBuffer'usage,
    p'rAudioBuffer'volume,
    p'rAudioProcessor'next,
    p'rAudioProcessor'prev,
    p'rAudioProcessor'process,
    p'sound'frameCount,
    p'sound'stream,
    p'wave'channels,
    p'wave'data,
    p'wave'frameCount,
    p'wave'sampleRate,
    p'wave'sampleSize,
  )

a'Wave'frameCount :: PtrAccessor Wave CUInt
a'Wave'frameCount = PtrAccessor p'wave'frameCount

a'Wave'sampleRate :: PtrAccessor Wave CUInt
a'Wave'sampleRate = PtrAccessor p'wave'sampleRate

a'Wave'sampleSize :: PtrAccessor Wave CUInt
a'Wave'sampleSize = PtrAccessor p'wave'sampleSize

a'Wave'channels :: PtrAccessor Wave CUInt
a'Wave'channels = PtrAccessor p'wave'channels

a'Wave'data :: PtrAccessor Wave (Ptr CShort)
a'Wave'data = PtrAccessor p'wave'data

a'RAudioBuffer'converter :: StaticArrayAccessor RAudioBuffer CUChar Int
a'RAudioBuffer'converter = staticArrayAccessor (castPtr . p'rAudioBuffer'converter) 312

a'RAudioBuffer'callback :: PtrAccessor RAudioBuffer C'AudioCallback
a'RAudioBuffer'callback = PtrAccessor p'rAudioBuffer'callback

a'RAudioBuffer'processor :: PtrAccessor RAudioBuffer (Maybe RAudioProcessor)
a'RAudioBuffer'processor = PtrAccessor (castPtr . p'rAudioBuffer'processor)

a'RAudioBuffer'volume :: PtrAccessor RAudioBuffer CFloat
a'RAudioBuffer'volume = PtrAccessor p'rAudioBuffer'volume

a'RAudioBuffer'pitch :: PtrAccessor RAudioBuffer CFloat
a'RAudioBuffer'pitch = PtrAccessor p'rAudioBuffer'pitch

a'RAudioBuffer'pan :: PtrAccessor RAudioBuffer CFloat
a'RAudioBuffer'pan = PtrAccessor p'rAudioBuffer'pan

a'RAudioBuffer'playing :: PtrAccessor RAudioBuffer CBool
a'RAudioBuffer'playing = PtrAccessor p'rAudioBuffer'playing

a'RAudioBuffer'paused :: PtrAccessor RAudioBuffer CBool
a'RAudioBuffer'paused = PtrAccessor p'rAudioBuffer'paused

a'RAudioBuffer'looping :: PtrAccessor RAudioBuffer CBool
a'RAudioBuffer'looping = PtrAccessor p'rAudioBuffer'looping

a'RAudioBuffer'usage :: PtrAccessor RAudioBuffer AudioBufferUsage
a'RAudioBuffer'usage = PtrAccessor p'rAudioBuffer'usage

a'RAudioBuffer'isSubBufferProcessed :: StaticArrayAccessor RAudioBuffer CBool Int
a'RAudioBuffer'isSubBufferProcessed = staticArrayAccessor (p'rAudioBuffer'isSubBufferProcessed) 2

a'RAudioBuffer'sizeInFrames :: PtrAccessor RAudioBuffer CUInt
a'RAudioBuffer'sizeInFrames = PtrAccessor p'rAudioBuffer'sizeInFrames

a'RAudioBuffer'frameCursorPos :: PtrAccessor RAudioBuffer CUInt
a'RAudioBuffer'frameCursorPos = PtrAccessor p'rAudioBuffer'frameCursorPos

a'RAudioBuffer'framesProcessed :: PtrAccessor RAudioBuffer CUInt
a'RAudioBuffer'framesProcessed = PtrAccessor p'rAudioBuffer'framesProcessed

a'RAudioBuffer'data :: PtrAccessor RAudioBuffer (Ptr CUChar)
a'RAudioBuffer'data = PtrAccessor p'rAudioBuffer'data

a'RAudioBuffer'next :: PtrAccessor RAudioBuffer (Maybe RAudioBuffer)
a'RAudioBuffer'next = PtrAccessor (castPtr . p'rAudioBuffer'next)

a'RAudioBuffer'prev :: PtrAccessor RAudioBuffer (Maybe RAudioBuffer)
a'RAudioBuffer'prev = PtrAccessor (castPtr . p'rAudioBuffer'prev)

a'RAudioProcessor'process :: PtrAccessor RAudioProcessor C'AudioCallback
a'RAudioProcessor'process = PtrAccessor p'rAudioProcessor'process

a'RAudioProcessor'next :: PtrAccessor RAudioProcessor (Maybe RAudioProcessor)
a'RAudioProcessor'next = PtrAccessor (castPtr . p'rAudioProcessor'next)

a'RAudioProcessor'prev :: PtrAccessor RAudioProcessor (Maybe RAudioProcessor)
a'RAudioProcessor'prev = PtrAccessor (castPtr . p'rAudioProcessor'prev)

a'AudioStream'buffer :: PtrAccessor AudioStream (Maybe RAudioBuffer)
a'AudioStream'buffer = PtrAccessor (castPtr . p'audioStream'buffer)

a'AudioStream'processor :: PtrAccessor AudioStream (Maybe RAudioProcessor)
a'AudioStream'processor = PtrAccessor (castPtr . p'audioStream'processor)

a'AudioStream'sampleRate :: PtrAccessor AudioStream CUInt
a'AudioStream'sampleRate = PtrAccessor p'audioStream'sampleRate

a'AudioStream'sampleSize :: PtrAccessor AudioStream CUInt
a'AudioStream'sampleSize = PtrAccessor p'audioStream'sampleSize

a'AudioStream'channels :: PtrAccessor AudioStream CUInt
a'AudioStream'channels = PtrAccessor p'audioStream'channels

a'Sound'stream :: PtrAccessor Sound AudioStream
a'Sound'stream = PtrAccessor p'sound'stream

a'Sound'frameCount :: PtrAccessor Sound CUInt
a'Sound'frameCount = PtrAccessor p'sound'frameCount

a'Music'stream :: PtrAccessor Music AudioStream
a'Music'stream = PtrAccessor p'music'stream

a'Music'frameCount :: PtrAccessor Music CUInt
a'Music'frameCount = PtrAccessor p'music'frameCount

a'Music'looping :: PtrAccessor Music CBool
a'Music'looping = PtrAccessor p'music'looping

a'Music'ctxType :: PtrAccessor Music MusicContextType
a'Music'ctxType = PtrAccessor p'music'ctxType

a'Music'ctxData :: PtrAccessor Music (Ptr ())
a'Music'ctxData = PtrAccessor p'music'ctxData
