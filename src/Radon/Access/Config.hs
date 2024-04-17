{-# LANGUAGE TemplateHaskellQuotes #-}

module Radon.Access.Config
  ( ptrAccessorConfig,
    referenceAccessorConfig,
    arrayAccessorConfig,
    arrayAccessorConfigFlip,
    fixedLengthArrayAccessorConfig,
    staticArrayAccessorConfig,
    capacityArrayAccessorConfig,
    capacityArrayAccessorConfigFlip,
    varCapacityArrayAccessorConfig,
    lastApostropheRenamer,
  )
where

import Foreign (Ptr, plusPtr)
import Radon.Access.Class (RStorable (rsizeOf))
import Language.Haskell.TH
  ( Exp (AppE, InfixE, LitE, SigE, VarE, ConE),
    Lit (IntegerL),
    Q,
    Type (AppT, ConT),
  )
import Radon.Access
  ( ArrayAccessor,
    CapacityArrayAccessor,
    PtrAccessor (..),
    ReferenceAccessor (..),
    StaticArrayAccessor,
    arrayAccessor,
    capacityArrayAccessor,
    staticArrayAccessor, VarCapacityArrayAccessor, varCapacityArrayAccessor, FixedLengthArrayAccessor, fixedLengthArrayAccessor,
  )
import Radon.Access.Gen (AccessorType (Array, Plain), Configurer)

ptrAccessorConfig :: Q Type -> Q Type -> Configurer
ptrAccessorConfig _p _c offset = do
  p <- _p
  c <- _c
  o <- offset
  return
    ( ConT ''PtrAccessor `AppT` p `AppT` c,
      ConE 'PtrAccessor `AppE` InfixE Nothing (VarE 'plusPtr) (Just o),
      VarE 'rsizeOf `AppE` SigE (VarE 'undefined) c,
      Plain c
    )

referenceAccessorConfig :: Q Type -> Q Type -> Configurer
referenceAccessorConfig _p _c offset = do
  p <- _p
  c <- _c
  o <- offset
  return
    ( ConT ''ReferenceAccessor `AppT` p `AppT` c,
      ConE 'ReferenceAccessor `AppE` InfixE Nothing (VarE 'plusPtr) (Just o),
      VarE 'rsizeOf `AppE` SigE (VarE 'undefined) (ConT ''Ptr `AppT` c),
      Plain c
    )

-- | Stores the size first, then the pointer
arrayAccessorConfig :: Q Type -> Q Type -> Q Type -> Configurer
arrayAccessorConfig _p _c _i offset = do
  p <- _p
  c <- _c
  i <- _i
  o <- offset
  return
    ( ConT ''ArrayAccessor `AppT` p `AppT` c `AppT` i,
      VarE 'arrayAccessor `AppE` InfixE Nothing (VarE 'plusPtr) (Just (InfixE (Just o) (VarE '(+)) (Just (VarE 'rsizeOf `AppE` SigE (VarE 'undefined) i)))) `AppE` InfixE Nothing (VarE 'plusPtr) (Just o),
      InfixE (Just (VarE 'rsizeOf `AppE` SigE (VarE 'undefined) i)) (VarE '(+)) (Just (VarE 'rsizeOf `AppE` SigE (VarE 'undefined) (ConT ''Ptr `AppT` c))),
      Array c
    )

-- | Stores the pointer first, then the size
arrayAccessorConfigFlip :: Q Type -> Q Type -> Q Type -> Configurer
arrayAccessorConfigFlip _p _c _i offset = do
  p <- _p
  c <- _c
  i <- _i
  o <- offset
  return
    ( ConT ''ArrayAccessor `AppT` p `AppT` c `AppT` i,
      VarE 'arrayAccessor `AppE` InfixE Nothing (VarE 'plusPtr) (Just o) `AppE` InfixE Nothing (VarE 'plusPtr) (Just (InfixE (Just o) (VarE '(+)) (Just (VarE 'rsizeOf `AppE` SigE (VarE 'undefined) (ConT ''Ptr `AppT` c))))),
      InfixE (Just (VarE 'rsizeOf `AppE` SigE (VarE 'undefined) i)) (VarE '(+)) (Just (VarE 'rsizeOf `AppE` SigE (VarE 'undefined) (ConT ''Ptr `AppT` c))),
      Array c
    )

fixedLengthArrayAccessorConfig :: Q Type -> Q Type -> Q Type -> Integer -> Configurer
fixedLengthArrayAccessorConfig _p _c _i size offset = do
  p <- _p
  c <- _c
  i <- _i
  o <- offset
  return
    ( ConT ''FixedLengthArrayAccessor `AppT` p `AppT` c `AppT` i,
      VarE 'fixedLengthArrayAccessor `AppE` InfixE Nothing (VarE 'plusPtr) (Just o) `AppE` LitE (IntegerL size),
      VarE 'rsizeOf `AppE` SigE (VarE 'undefined) (ConT ''Ptr `AppT` c),
      Array c
    )

staticArrayAccessorConfig :: Q Type -> Q Type -> Q Type -> Integer -> Configurer
staticArrayAccessorConfig _p _c _i size offset = do
  p <- _p
  c <- _c
  i <- _i
  o <- offset
  return
    ( ConT ''StaticArrayAccessor `AppT` p `AppT` c `AppT` i,
      VarE 'staticArrayAccessor `AppE` InfixE Nothing (VarE 'plusPtr) (Just o) `AppE` LitE (IntegerL size),
      InfixE (Just (VarE 'rsizeOf `AppE` SigE (VarE 'undefined) c)) (VarE '(*)) (Just (LitE (IntegerL size))),
      Array c
    )

capacityArrayAccessorConfig :: Q Type -> Q Type -> Q Type -> Integer -> Configurer
capacityArrayAccessorConfig _p _c _i capacity offset = do
  p <- _p
  c <- _c
  i <- _i
  o <- offset
  return
    ( ConT ''CapacityArrayAccessor `AppT` p `AppT` c `AppT` i,
      VarE 'capacityArrayAccessor `AppE` InfixE Nothing (VarE 'plusPtr) (Just (InfixE (Just o) (VarE '(+)) (Just (VarE 'rsizeOf `AppE` SigE (VarE 'undefined) i)))) `AppE` InfixE Nothing (VarE 'plusPtr) (Just o) `AppE` LitE (IntegerL capacity),
      InfixE (Just (VarE 'rsizeOf `AppE` SigE (VarE 'undefined) i)) (VarE '(+)) (Just (VarE 'rsizeOf `AppE` SigE (VarE 'undefined) (ConT ''Ptr `AppT` c))),
      Array c
    )

capacityArrayAccessorConfigFlip :: Q Type -> Q Type -> Q Type -> Integer -> Configurer
capacityArrayAccessorConfigFlip _p _c _i capacity offset = do
  p <- _p
  c <- _c
  i <- _i
  o <- offset
  return
    ( ConT ''CapacityArrayAccessor `AppT` p `AppT` c `AppT` i,
      VarE 'capacityArrayAccessor `AppE` InfixE Nothing (VarE 'plusPtr) (Just o) `AppE` InfixE Nothing (VarE 'plusPtr) (Just (InfixE (Just o) (VarE '(+)) (Just (VarE 'rsizeOf `AppE` SigE (VarE 'undefined) (ConT ''Ptr `AppT` c))))) `AppE` LitE (IntegerL capacity),
      InfixE (Just (VarE 'rsizeOf `AppE` SigE (VarE 'undefined) i)) (VarE '(+)) (Just (VarE 'rsizeOf `AppE` SigE (VarE 'undefined) (ConT ''Ptr `AppT` c))),
      Array c
    )

varCapacityArrayAccessorConfig :: Q Type -> Q Type -> Q Type -> Configurer
varCapacityArrayAccessorConfig _p _c _i offset = do
  p <- _p
  c <- _c
  i <- _i
  o <- offset
  return
    ( ConT ''VarCapacityArrayAccessor `AppT` p `AppT` c `AppT` i,
      VarE 'varCapacityArrayAccessor `AppE` InfixE Nothing (VarE 'plusPtr) (Just (InfixE (Just o) (VarE '(+)) (Just (InfixE (Just (VarE 'rsizeOf `AppE` SigE (VarE 'undefined) i)) (VarE '(*)) (Just (LitE (IntegerL 2))))))) `AppE` InfixE Nothing (VarE 'plusPtr) (Just (InfixE (Just o) (VarE '(+)) (Just (VarE 'rsizeOf `AppE` SigE (VarE 'undefined) i)))) `AppE` InfixE Nothing (VarE 'plusPtr) (Just o),
      InfixE (Just (InfixE (Just (VarE 'rsizeOf `AppE` SigE (VarE 'undefined) i)) (VarE '(*)) (Just (LitE (IntegerL 2))))) (VarE '(+)) (Just (VarE 'rsizeOf `AppE` SigE (VarE 'undefined) (ConT ''Ptr `AppT` c))),
      Array c
    )

lastApostropheRenamer :: String -> String
lastApostropheRenamer = last <$> uncurry (:) <$> foldr (\c (current, chunks) -> case c of '\'' -> ("", current:chunks); _ -> (c:current, chunks)) ("", [])
