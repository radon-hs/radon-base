{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Radon.Access
  ( PAccessor (..),
    (>.>),
    Accessor (..),
    (>.),
    PtrAccessor (PtrAccessor),
    idPtrAccessor,
    ReferenceAccessor (ReferenceAccessor),
    referenceToPtrAccessor,
    ArrayLike (..),
    (>>.),
    ArrayAccessor (ArrayAccessor),
    arrayAccessor,
    MaybeArrayAccessor (MaybeArrayAccessor),
    maybeArrayAccessor,
    FixedLengthArrayAccessor (FixedLengthArrayAccessor),
    fixedLengthArrayAccessor,
    StaticArrayAccessor (StaticArrayAccessor),
    staticArrayAccessor,
    CapacityArrayAccessor (CapacityArrayAccessor),
    capacityArrayAccessor,
    VarCapacityArrayAccessor (VarCapacityArrayAccessor),
    varCapacityArrayAccessor,
  )
where

import Control.Monad (when, (<=<), (>=>), unless)
import Foreign (Ptr, free, nullPtr)
import Radon.Access.Class (Freeable (..))
import Radon.Access.Class
  ( RStorable (rpeek, rpoke),
    radvancePtr,
    rcalloc,
    rcallocArray,
    rpeekArray,
    rpokeArray,
    rreallocArray,
  )

-- * Primitive

newtype PAccessor p c = PAccessor {runP :: Ptr p -> IO (Ptr c)}

infixl 9 >.>

(>.>) :: PAccessor p c1 -> PAccessor c1 c2 -> PAccessor p c2
(>.>) (PAccessor a) (PAccessor b) = PAccessor (a >=> b)

-- * High level

-- parent.field :: Ptr p -> Ptr c (PtrAccessor)
-- parent->field :: Ptr p -> IO (Ptr c) (PAccessor)
-- (int *arr) arr[n] :: Ptr (Ptr p) -> IO (Ptr p) (ArrayAccessor/CapacityArrayAccessor)
-- (int arr[n]) arr[n] :: Ptr p -> Ptr p (StaticArrayAccessor)

class Accessor a where
  toPrim :: a p c -> PAccessor p c

  runAccessor :: (RStorable c) => a p c -> Ptr p -> IO (Ptr c)
  runAccessor = runP . toPrim

  readAccessor :: (RStorable c) => a p c -> Ptr p -> IO c
  readAccessor a p = rpeek =<< runP (toPrim a) p

  writeAccessor :: (RStorable c, Freeable c) => a p c -> Ptr p -> c -> IO ()
  writeAccessor a p v = do
    ptr <- runP (toPrim a) p
    freeDependents ptr
    rpoke ptr v

  modifyAccessor :: (RStorable c, Freeable c) => a p c -> Ptr p -> (c -> c) -> IO ()
  modifyAccessor a p f = do
    r <- readAccessor a p
    writeAccessor a p (f r)

  modifyAccessorIO :: (RStorable c, Freeable c) => a p c -> Ptr p -> (c -> IO c) -> IO ()
  modifyAccessorIO a p f = do
    v <- readAccessor a p
    v' <- f v
    writeAccessor a p v'

  freeAccessor :: (RStorable c, Freeable c) => a p c -> Ptr p -> IO ()
  freeAccessor a p = freeDependents =<< runAccessor a p

instance Accessor PAccessor where
  toPrim = id

infixl 9 >.

(>.) :: (Accessor a, Accessor b) => a p c1 -> b c1 c2 -> PAccessor p c2
(>.) a b = toPrim a >.> toPrim b

newtype PtrAccessor p c = PtrAccessor (Ptr p -> Ptr c)

instance Accessor PtrAccessor where
  toPrim (PtrAccessor f) = PAccessor (return . f)

idPtrAccessor :: PtrAccessor a a
idPtrAccessor = PtrAccessor id

newtype ReferenceAccessor p c = ReferenceAccessor (Ptr p -> Ptr (Ptr c))

instance Accessor ReferenceAccessor where
  toPrim (ReferenceAccessor f) = PAccessor (rpeek . f)

  runAccessor (ReferenceAccessor f) p = do
    let refPtr = f p
    ptr <- rpeek refPtr
    if ptr == nullPtr
      then do
        ptr' <- rcalloc
        rpoke refPtr ptr'
        return ptr'
      else return ptr

  freeAccessor (ReferenceAccessor f) p = do
    ptr <- rpeek (f p)
    freeDependents ptr
    free ptr

referenceToPtrAccessor :: ReferenceAccessor p c -> PtrAccessor p (Ptr c)
referenceToPtrAccessor (ReferenceAccessor f) = PtrAccessor f

-- * Arrays

class ArrayLike a where
  type ArrayIndex a i
  type ArrayRep a c
  type ArrayPtrRep a c

  advanceArray :: (RStorable c, Integral i) => ArrayIndex a i -> a p c i -> PAccessor p c
  chainArray :: (RStorable c1, RStorable c2, Accessor b) => b p c1 -> a c1 c2 i -> a p c2 i

  readArrayLength :: (RStorable i) => a p c i -> Ptr p -> IO (ArrayIndex a i)
  writeArrayLength :: (RStorable c, Freeable c, RStorable i, Integral i) => a p c i -> Ptr p -> ArrayIndex a i -> IO ()

  runArray :: (RStorable c) => a p c i -> Ptr p -> IO (ArrayPtrRep a c)

  readArray :: (RStorable c, Integral i, RStorable i) => a p c i -> Ptr p -> IO (ArrayRep a c)

  writeArray :: (RStorable c, Freeable c, Integral i, RStorable i) => a p c i -> Ptr p -> ArrayRep a c -> IO ()

  modifyArray :: (RStorable c, Freeable c, Integral i, RStorable i) => a p c i -> Ptr p -> (ArrayRep a c -> ArrayRep a c) -> IO ()
  modifyArray a p f = do
    v <- readArray a p
    writeArray a p (f v)

  modifyArrayIO :: (RStorable c, Freeable c, Integral i, RStorable i) => a p c i -> Ptr p -> (ArrayRep a c -> IO (ArrayRep a c)) -> IO ()
  modifyArrayIO a p f = do
    v <- readArray a p
    v' <- f v
    writeArray a p v'

  freeArray :: (RStorable c, Freeable c, RStorable i, Integral i) => a p c i -> Ptr p -> IO ()

infixl 9 >>.

(>>.) :: (ArrayLike a, Accessor b, RStorable c1, RStorable c2) => b p c1 -> a c1 c2 i -> a p c2 i
(>>.) = chainArray

data ArrayAccessor p c i = ArrayAccessor (Ptr p -> IO (Ptr (Ptr c))) (Ptr p -> IO (Ptr i))

arrayAccessor :: (Integral i) => (Ptr p -> Ptr (Ptr c)) -> (Ptr p -> Ptr i) -> ArrayAccessor p c i
arrayAccessor f l = ArrayAccessor (return . f) (return . l)

instance ArrayLike ArrayAccessor where
  type ArrayIndex ArrayAccessor i = i
  type ArrayRep ArrayAccessor c = [c]
  type ArrayPtrRep ArrayAccessor c = Ptr c

  advanceArray i (ArrayAccessor f _) =
    PAccessor
      ( \p -> do
          arrPtr <- rpeek =<< f p
          return (radvancePtr arrPtr (fromIntegral i))
      )
  chainArray a (ArrayAccessor f l) = ArrayAccessor (runAccessor a >=> f) (runAccessor a >=> l)

  readArrayLength (ArrayAccessor _ l) = rpeek <=< l
  writeArrayLength (ArrayAccessor f l) p nl = do
    ptr <- f p
    arr <- rpeek ptr
    lptr <- l p
    len <- rpeek lptr
    when (nl < len) $ mapM_ (freeDependents . radvancePtr arr) [fromIntegral nl .. fromIntegral len - 1]
    when (nl > len) $ rpoke ptr =<< rreallocArray arr (fromIntegral nl)
    rpoke lptr nl

  runArray (ArrayAccessor f _) = rpeek <=< f
  readArray a p = do
    l <- readArrayLength a p
    arr <- runArray a p
    rpeekArray (fromIntegral l) arr
  writeArray a@(ArrayAccessor f _) p vs = do
    len <- readArrayLength a p
    writeArrayLength a p (fromIntegral (length vs))
    ptr <- f p
    arr <- rpeek ptr
    mapM_ (freeDependents . radvancePtr arr) [0 .. fromIntegral len - 1]
    rpokeArray arr vs
  freeArray a@(ArrayAccessor f _) p = do
    len <- readArrayLength a p
    ptr <- f p
    arr <- rpeek ptr
    mapM_ (freeDependents . radvancePtr arr) [0 .. fromIntegral len - 1]
    free arr

data MaybeArrayAccessor p c i = MaybeArrayAccessor (Ptr p -> IO (Ptr (Ptr c))) (Ptr p -> IO (Ptr i))

maybeArrayAccessor :: (Integral i) => (Ptr p -> Ptr (Ptr c)) -> (Ptr p -> Ptr i) -> MaybeArrayAccessor p c i
maybeArrayAccessor f l = MaybeArrayAccessor (return . f) (return . l)

instance ArrayLike MaybeArrayAccessor where
  type ArrayIndex MaybeArrayAccessor i = i
  type ArrayRep MaybeArrayAccessor c = Maybe [c]
  type ArrayPtrRep MaybeArrayAccessor c = Ptr c

  advanceArray i (MaybeArrayAccessor f _) =
    PAccessor
      ( \p -> do
          arrPtr <- rpeek =<< f p
          return (radvancePtr arrPtr (fromIntegral i))
      )
  chainArray a (MaybeArrayAccessor f l) = MaybeArrayAccessor (runAccessor a >=> f) (runAccessor a >=> l)

  readArrayLength (MaybeArrayAccessor _ l) = rpeek <=< l
  writeArrayLength (MaybeArrayAccessor f l) p nl = do
    ptr <- f p
    arr <- rpeek ptr
    lptr <- l p
    len <- rpeek lptr
    when (nl < len) $ mapM_ (freeDependents . radvancePtr arr) [fromIntegral nl .. fromIntegral len - 1]
    when (nl > len) $ rpoke ptr =<< rreallocArray arr (fromIntegral nl)
    rpoke lptr nl

  runArray (MaybeArrayAccessor f _) = rpeek <=< f
  readArray a p = do
    l <- readArrayLength a p
    arr <- runArray a p
    if arr == nullPtr then return Nothing else Just <$> rpeekArray (fromIntegral l) arr
  writeArray a@(MaybeArrayAccessor f _) p m = do
    len <- readArrayLength a p
    ptr <- f p
    arr <- rpeek ptr
    unless (arr == nullPtr) $ mapM_ (freeDependents . radvancePtr arr) [0 .. fromIntegral len - 1]
    case m of
      Nothing -> do
        writeArrayLength a p 0
        rpoke ptr nullPtr
      Just vs -> do
        writeArrayLength a p (fromIntegral (length vs))
        arr' <- rpeek ptr
        rpokeArray arr' vs
  freeArray a@(MaybeArrayAccessor f _) p = do
    len <- readArrayLength a p
    ptr <- f p
    arr <- rpeek ptr
    mapM_ (freeDependents . radvancePtr arr) [0 .. fromIntegral len - 1]
    free arr

data FixedLengthArrayAccessor p c i = FixedLengthArrayAccessor (Ptr p -> IO (Ptr (Ptr c))) i

fixedLengthArrayAccessor :: (Ptr p -> Ptr (Ptr c)) -> i -> FixedLengthArrayAccessor p c i
fixedLengthArrayAccessor f = FixedLengthArrayAccessor (return . f)

instance ArrayLike FixedLengthArrayAccessor where
  type ArrayIndex FixedLengthArrayAccessor i = i
  type ArrayRep FixedLengthArrayAccessor c = [c]
  type ArrayPtrRep FixedLengthArrayAccessor c = Ptr c

  advanceArray i (FixedLengthArrayAccessor f _) =
    PAccessor
      ( \p -> do
          arrPtr <- rpeek =<< f p
          return (radvancePtr arrPtr (fromIntegral i))
      )
  chainArray a (FixedLengthArrayAccessor f l) = FixedLengthArrayAccessor (runAccessor a >=> f) l

  readArrayLength (FixedLengthArrayAccessor _ l) _ = return l
  writeArrayLength _ _ _ = fail "cannot set length of fixed length array!"

  runArray (FixedLengthArrayAccessor f _) = rpeek <=< f
  readArray a p = do
    l <- readArrayLength a p
    arr <- runArray a p
    rpeekArray (fromIntegral l) arr
  writeArray (FixedLengthArrayAccessor f l) p vs = do
    if length vs /= fromIntegral l
      then fail "incorrect array length for fixed length array"
      else do
        ptr <- f p
        arr <- rpeek ptr
        mapM_ (freeDependents . radvancePtr arr) [0 .. fromIntegral l - 1]
        rpokeArray arr vs
  freeArray (FixedLengthArrayAccessor f l) p = do
    ptr <- f p
    arr <- rpeek ptr
    mapM_ (freeDependents . radvancePtr arr) [0 .. fromIntegral l - 1]
    free arr

data StaticArrayAccessor p c i = StaticArrayAccessor (Ptr p -> IO (Ptr c)) i

staticArrayAccessor :: (Ptr p -> Ptr c) -> i -> StaticArrayAccessor p c i
staticArrayAccessor f = StaticArrayAccessor (return . f)

instance ArrayLike StaticArrayAccessor where
  type ArrayIndex StaticArrayAccessor i = i
  type ArrayRep StaticArrayAccessor c = [c]
  type ArrayPtrRep StaticArrayAccessor c = Ptr c

  advanceArray i (StaticArrayAccessor f _) =
    PAccessor
      ( \p -> do
          arrPtr <- f p
          return (radvancePtr arrPtr (fromIntegral i))
      )
  chainArray a (StaticArrayAccessor f l) = StaticArrayAccessor (runAccessor a >=> f) l

  readArrayLength (StaticArrayAccessor _ l) = const (return l)
  writeArrayLength _ _ _ = fail "cannot set length of static array!"

  runArray (StaticArrayAccessor f _) = f
  readArray a p = do
    l <- readArrayLength a p
    arr <- runArray a p
    rpeekArray (fromIntegral l) arr
  writeArray (StaticArrayAccessor f l) p vs = do
    if length vs /= fromIntegral l
      then fail "incorrect array length for static array"
      else do
        ptr <- f p
        mapM_ (freeDependents . radvancePtr ptr) [0 .. fromIntegral l - 1]
        rpokeArray ptr vs
  freeArray (StaticArrayAccessor f l) p = do
    arr <- f p
    mapM_ (freeDependents . radvancePtr arr) [0 .. fromIntegral l - 1]

data CapacityArrayAccessor p c i = CapacityArrayAccessor (Ptr p -> IO (Ptr (Ptr c))) (Ptr p -> IO (Ptr i)) i

capacityArrayAccessor :: (Ptr p -> Ptr (Ptr c)) -> (Ptr p -> Ptr i) -> i -> CapacityArrayAccessor p c i
capacityArrayAccessor f l = CapacityArrayAccessor (return . f) (return . l)

instance ArrayLike CapacityArrayAccessor where
  type ArrayIndex CapacityArrayAccessor i = i
  type ArrayRep CapacityArrayAccessor c = [c]
  type ArrayPtrRep CapacityArrayAccessor c = Ptr c

  advanceArray i (CapacityArrayAccessor f _ _) =
    PAccessor
      ( \p -> do
          arrPtr <- rpeek =<< f p
          return (radvancePtr arrPtr (fromIntegral i))
      )
  chainArray a (CapacityArrayAccessor f l c) = CapacityArrayAccessor (runAccessor a >=> f) (runAccessor a >=> l) c

  readArrayLength (CapacityArrayAccessor _ l _) = rpeek <=< l
  writeArrayLength (CapacityArrayAccessor f l _) p nl = do
    ptr <- f p
    arr <- rpeek ptr
    lptr <- l p
    len <- rpeek lptr
    when (nl < len) $ mapM_ (freeDependents . radvancePtr arr) [fromIntegral nl .. fromIntegral len - 1]
    rpoke lptr nl

  runArray (CapacityArrayAccessor f _ _) = rpeek <=< f
  readArray a p = do
    l <- readArrayLength a p
    arr <- runArray a p
    rpeekArray (fromIntegral l) arr
  writeArray (CapacityArrayAccessor f l c) p vs = do
    ptr <- f p
    arr <- rpeek ptr
    lptr <- l p
    arr' <-
      if arr == nullPtr
        then do
          newArr <- rcallocArray (fromIntegral c)
          rpoke ptr newArr
          return newArr
        else return arr
    rpoke lptr (fromIntegral (length vs))
    rpokeArray arr' vs
  freeArray a@(CapacityArrayAccessor f _ _) p = do
    ptr <- f p
    arr <- rpeek ptr
    len <- readArrayLength a p
    mapM_ (freeDependents . radvancePtr arr) [0 .. fromIntegral len - 1]
    free arr

data VarCapacityArrayAccessor p c i = VarCapacityArrayAccessor (Ptr p -> IO (Ptr (Ptr c))) (Ptr p -> IO (Ptr i)) (Ptr p -> IO (Ptr i))

varCapacityArrayAccessor :: (Ptr p -> Ptr (Ptr c)) -> (Ptr p -> Ptr i) -> (Ptr p -> Ptr i) -> VarCapacityArrayAccessor p c i
varCapacityArrayAccessor f l i = VarCapacityArrayAccessor (return . f) (return . l) (return . i)

instance ArrayLike VarCapacityArrayAccessor where
  type ArrayIndex VarCapacityArrayAccessor i = i
  type ArrayRep VarCapacityArrayAccessor c = [c]
  type ArrayPtrRep VarCapacityArrayAccessor c = Ptr c

  advanceArray i (VarCapacityArrayAccessor f _ _) =
    PAccessor
      ( \p -> do
          arrPtr <- rpeek =<< f p
          return (radvancePtr arrPtr (fromIntegral i))
      )
  chainArray a (VarCapacityArrayAccessor f l c) = VarCapacityArrayAccessor (runAccessor a >=> f) (runAccessor a >=> l) (runAccessor a >=> c)

  readArrayLength (VarCapacityArrayAccessor _ l _) = rpeek <=< l
  writeArrayLength (VarCapacityArrayAccessor f l _) p nl = do
    ptr <- f p
    arr <- rpeek ptr
    lptr <- l p
    len <- rpeek lptr
    when (nl < len) $ mapM_ (freeDependents . radvancePtr arr) [fromIntegral nl .. fromIntegral len - 1]
    rpoke lptr nl

  runArray (VarCapacityArrayAccessor f _ _) = rpeek <=< f
  readArray a p = do
    l <- readArrayLength a p
    arr <- runArray a p
    rpeekArray (fromIntegral l) arr
  writeArray (VarCapacityArrayAccessor f l c) p vs = do
    ptr <- f p
    arr <- rpeek ptr
    lptr <- l p
    arr' <-
      if arr == nullPtr
        then do
          cptr <- c p
          cap <- rpeek cptr
          newArr <- rcallocArray (fromIntegral cap)
          rpoke ptr newArr
          return newArr
        else return arr
    rpoke lptr (fromIntegral (length vs))
    rpokeArray arr' vs
  freeArray a@(VarCapacityArrayAccessor f _ _) p = do
    ptr <- f p
    arr <- rpeek ptr
    len <- readArrayLength a p
    mapM_ (freeDependents . radvancePtr arr) [0 .. fromIntegral len - 1]
    free arr
