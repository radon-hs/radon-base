{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Radon.Access.Class
  ( Freeable (..),
    RStorable (..),
    radvancePtr,
    rcalloc,
    rclearBytes,
    rcallocArray,
    rcallocArray0,
    rreallocArray,
    rreallocArray0,
    rpeekArray,
    rpokeArray,
    rpokeArray0,
    rnewArray,
    rnewArray0
  )
where

import Control.Monad (forM, forM_, unless, when)
import qualified Data.Either as E
import Foreign (Ptr, Storable (..), Word16, Word32, Word64, Word8, callocBytes, castPtr, fillBytes, free, nullPtr, plusPtr, reallocBytes)
import Foreign.C (CBool, CChar, CDouble, CFloat, CInt, CLong, CShort, CString, CUChar, CUInt, CULong, CUShort, castCharToCChar, peekCString)
import GHC.Base (Int (I#), (+#))
import Radon.Internal.Math (highestPowerOfTwo)
import Radon.Internal.TH (genTupleInstances)
import qualified Raylib.Util as R

class Freeable a where
  freeDependents :: Ptr a -> IO ()
  freeDependents _ = return ()

class RStorable a where
  {-# MINIMAL
    rsizeOf,
    ralignment,
    (rpeek | rpeekElemOff | rpeekByteOff),
    (rpoke | rpokeElemOff | rpokeByteOff)
    #-}

  ralignment :: a -> Int
  rsizeOf :: a -> Int
  rpeek :: Ptr a -> IO a
  rpoke :: Ptr a -> a -> IO ()
  rpeekElemOff :: Ptr a -> Int -> IO a
  rpokeElemOff :: Ptr a -> Int -> a -> IO ()
  rpeekByteOff :: Ptr b -> Int -> IO a
  rpokeByteOff :: Ptr b -> Int -> a -> IO ()

  rpeekElemOff ptr off = rpeekByteOff ptr (off * rsizeOf (undefined :: a))
  rpokeElemOff ptr off val = rpokeByteOff ptr (off * rsizeOf val) val

  rpeekByteOff ptr off = rpeek (ptr `plusPtr` off)
  rpokeByteOff ptr off = rpoke (ptr `plusPtr` off)

  rpeek ptr = rpeekElemOff ptr 0
  rpoke ptr = rpokeElemOff ptr 0

radvancePtr :: forall a. (RStorable a) => Ptr a -> Int -> Ptr a
{-# INLINE radvancePtr #-}
radvancePtr ptr i = ptr `plusPtr` (i * rsizeOf (undefined :: a))

rcalloc :: forall a. (RStorable a) => IO (Ptr a)
{-# INLINE rcalloc #-}
rcalloc = callocBytes (rsizeOf (undefined :: a))

rclearBytes :: Int -> Ptr a -> IO ()
{-# INLINE rclearBytes #-}
rclearBytes bytes ptr = fillBytes ptr 0 bytes

rcallocArray :: forall a. (RStorable a) => Int -> IO (Ptr a)
{-# INLINE rcallocArray #-}
rcallocArray size = callocBytes (size * rsizeOf (undefined :: a))

rcallocArray0 :: (RStorable a) => Int -> IO (Ptr a)
{-# INLINE rcallocArray0 #-}
rcallocArray0 size = rcallocArray (size + 1)

rreallocArray :: forall a. (RStorable a) => Ptr a -> Int -> IO (Ptr a)
{-# INLINE rreallocArray #-}
rreallocArray ptr size = reallocBytes ptr (size * rsizeOf (undefined :: a))

rreallocArray0 :: (RStorable a) => Ptr a -> Int -> IO (Ptr a)
{-# INLINE rreallocArray0 #-}
rreallocArray0 ptr size = rreallocArray ptr (size + 1)

rpeekArray :: (RStorable a) => Int -> Ptr a -> IO [a]
{-# INLINEABLE rpeekArray #-}
rpeekArray size ptr
  | size <= 0 = return []
  | otherwise = f (size - 1) []
  where
    f 0 acc = do e <- rpeekElemOff ptr 0; return (e : acc)
    f n acc = do e <- rpeekElemOff ptr n; f (n - 1) (e : acc)

rpokeArray :: (RStorable a) => Ptr a -> [a] -> IO ()
{-# INLINEABLE rpokeArray #-}
rpokeArray ptr vals0 = go vals0 0#
  where
    go [] _ = return ()
    go (val : vals) n# = do rpokeElemOff ptr (I# n#) val; go vals (n# +# 1#)

rpokeArray0 :: (RStorable a) => a -> Ptr a -> [a] -> IO ()
{-# INLINEABLE rpokeArray0 #-}
rpokeArray0 marker ptr vals0 = go vals0 0#
  where
    go [] n# = rpokeElemOff ptr (I# n#) marker
    go (val : vals) n# = do rpokeElemOff ptr (I# n#) val; go vals (n# +# 1#)

rnewArray :: (RStorable a) => [a] -> IO (Ptr a)
{-# INLINEABLE rnewArray #-}
rnewArray vals = do
  ptr <- rcallocArray (length vals)
  rpokeArray ptr vals
  return ptr

rnewArray0 :: (RStorable a) => a -> [a] -> IO (Ptr a)
{-# INLINEABLE rnewArray0 #-}
rnewArray0 marker vals = do
  ptr <- rcallocArray0 (length vals)
  rpokeArray0 marker ptr vals
  return ptr

instance Freeable Bool

instance Freeable Float

instance Freeable Double

instance Freeable Char

instance Freeable Int

instance Freeable Integer

instance Freeable Word

instance Freeable Word8

instance Freeable Word16

instance Freeable Word32

instance Freeable Word64

instance Freeable CBool

instance Freeable CChar

instance Freeable CUChar

instance Freeable CShort

instance Freeable CUShort

instance Freeable CInt

instance Freeable CUInt

instance Freeable CLong

instance Freeable CULong

instance Freeable CFloat

instance Freeable CDouble

instance (Freeable a) => Freeable (Ptr a) where
  freeDependents p = do
    ptr <- rpeek p
    freeDependents ptr
    free ptr

instance {-# INCOHERENT #-} (RStorable a, R.Freeable a) => Freeable a where
  freeDependents p = rpeek p >>= (`R.rlFreeDependents` p)

instance {-# INCOHERENT #-} (Storable a) => RStorable a where
  ralignment = alignment
  rsizeOf = sizeOf
  rpeek = peek
  rpoke = poke

instance {-# OVERLAPPABLE #-} (RStorable a, Freeable a) => RStorable [a] where
  ralignment = highestPowerOfTwo . rsizeOf
  rsizeOf _ = rsizeOf (0 :: Int) + rsizeOf (undefined :: Ptr a)
  rpeek ptr = do
    size <- rpeek (castPtr ptr)
    arr <- rpeek (ptr `plusPtr` rsizeOf size)
    rpeekArray size arr
  rpoke ptr arr = do
    size <- rpeek (castPtr ptr)
    print size
    p' <- rpeek (ptr `plusPtr` rsizeOf (0 :: Int))
    print p'
    p <- rreallocArray p' (length arr)
    print p
    rpoke (castPtr ptr) (length arr)
    putStrLn "poked length"
    forM_ [0 .. size - 1] (\i -> freeDependents (radvancePtr p i))
    putStrLn "freed each"
    rpoke (ptr `plusPtr` rsizeOf (0 :: Int)) p
    putStrLn "poked"
    rpokeArray p arr

instance {-# OVERLAPPABLE #-} (RStorable a, Freeable a) => Freeable [a] where
  freeDependents ptr = do
    size <- rpeek (castPtr ptr)
    arr <- rpeek (ptr `plusPtr` rsizeOf size) :: IO (Ptr a)
    forM_ [0 .. size - 1] (\i -> freeDependents (radvancePtr arr i))
    free arr

instance (RStorable a, Freeable a) => RStorable (Maybe a) where
  ralignment _ = ralignment (undefined :: a)
  rsizeOf _ = rsizeOf (undefined :: a)
  rpeek ptr = do
    p <- rpeek (castPtr ptr)
    if p == nullPtr
      then return Nothing
      else Just <$> rpeek p
  rpoke ptr val = do
    let ptr' = castPtr ptr :: Ptr (Ptr a)
    p <- rpeek ptr'
    case val of
      Nothing -> when (p /= nullPtr) (freeDependents p >> free p >> rpoke ptr' nullPtr)
      Just v -> do
        p' <-
          if (p == nullPtr)
            then do
              newp <- rcalloc
              rpoke ptr' newp
              return newp
            else do
              freeDependents p
              rclearBytes (rsizeOf (undefined :: a)) p
              return p
        rpoke p' v

instance (Freeable a) => Freeable (Maybe a) where
  freeDependents ptr = do
    p <- rpeek (castPtr ptr :: Ptr (Ptr a))
    unless (p == nullPtr) (freeDependents p >> free p)

instance (RStorable a, RStorable b, Freeable a, Freeable b) => RStorable (Either a b) where
  ralignment v = highestPowerOfTwo (rsizeOf True + rsizeOf v)
  rsizeOf _ = rsizeOf True + max (rsizeOf (undefined :: a)) (rsizeOf (undefined :: b))
  rpeek ptr = do
    isLeft <- rpeek (castPtr ptr)
    if isLeft
      then Left <$> rpeek (ptr `plusPtr` rsizeOf True)
      else Right <$> rpeek (ptr `plusPtr` rsizeOf True)
  rpoke ptr val = do
    isLeft <- rpeek (castPtr ptr)
    if isLeft
      then freeDependents (ptr `plusPtr` rsizeOf True :: Ptr a)
      else freeDependents (ptr `plusPtr` rsizeOf True :: Ptr b)
    -- clear memory
    rclearBytes (max (rsizeOf (undefined :: a)) (rsizeOf (undefined :: b))) (ptr `plusPtr` rsizeOf True)
    rpoke (castPtr ptr) (E.isLeft val)
    case val of
      Left v -> rpoke (ptr `plusPtr` rsizeOf True) v
      Right v -> rpoke (ptr `plusPtr` rsizeOf True) v

instance (RStorable a, RStorable b, Freeable a, Freeable b) => Freeable (Either a b) where
  freeDependents ptr = do
    isLeft <- rpeek (castPtr ptr)
    if isLeft
      then freeDependents (ptr `plusPtr` rsizeOf True :: Ptr a)
      else freeDependents (ptr `plusPtr` rsizeOf True :: Ptr b)

instance RStorable String where
  ralignment = highestPowerOfTwo . rsizeOf
  rsizeOf _ = rsizeOf (undefined :: CString)
  rpeek ptr = do
    p <- rpeek (castPtr ptr)
    if p == nullPtr then return "" else peekCString p
  rpoke ptr val = do
    free =<< rpeek (castPtr ptr :: Ptr CString)
    p' <- rcallocArray (length val + 1)
    rpokeArray0 0 p' (map castCharToCChar val)
    rpoke (castPtr ptr) p'

instance Freeable String where
  freeDependents ptr = free =<< rpeek (castPtr ptr :: Ptr CString)

concat <$> forM [2 .. 10] genTupleInstances
