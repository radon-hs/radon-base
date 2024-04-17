{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Radon.Access.Gen
  ( AccessorType (..),
    CustomConfig,
    Configurer,
    DefaultConfigurer,
    Renamer,
    genAccessors,
    genAccessorsWith,
    genStorable,
    genStorableWith,
    genFreeable,
    genFreeableWith,
    genAll,
    genAllWith,
  )
where

import Control.Monad (forM)
import Data.Foldable (foldlM)
import Data.List (find)
import Foreign (plusPtr, Storable (..))
import Language.Haskell.TH (Body (NormalB), Clause (Clause), Con (GadtC, InfixC, NormalC, RecC, RecGadtC), Dec (DataD, FunD, InstanceD, SigD, TySynD), DecsQ, Exp (AppE, CaseE, ConE, DoE, InfixE, ListE, LitE, SigE, VarE), Info (TyConI), Lit (IntegerL), Match (Match), Name, Pat (ConP, LitP, VarP, WildP), Stmt (BindS, NoBindS), Type (AppT, ConT, ListT), isInstance, mkName, nameBase, reify, Ppr (ppr))
import Language.Haskell.TH.Syntax (Q)
import Radon.Access (Accessor (freeAccessor), ArrayAccessor, ArrayLike (freeArray), PtrAccessor (PtrAccessor), arrayAccessor, readAccessor, readArray, writeAccessor, writeArray)
import Radon.Access.Class (Freeable (freeDependents))
import Radon.Access.Class (RStorable (rsizeOf))
import Radon.Internal.Math (highestPowerOfTwo)

data AccessorType = Plain Type | Array Type deriving (Show)

type ConFields = (Type, Name, [(String, Type)])

data FieldOffsets = Single (Type, Name, [(Exp, String, Type, AccessorType)], Exp) | Multiple Int [(Type, Name, [(Exp, String, Type, AccessorType)], Exp)] deriving (Show)

type Configurer = Q Exp -> Q (Type, Exp, Exp, AccessorType)

type DefaultConfigurer = Configurer

type CustomConfig = (Maybe DefaultConfigurer, [(Name, Configurer)])

type Renamer = String -> String

get :: String -> CustomConfig -> Maybe Configurer
get str config = snd <$> find (\(name, _) -> nameBase name == str) (snd config)

getDefault :: CustomConfig -> Maybe DefaultConfigurer
getDefault = fst

genAccessors :: Name -> DecsQ
genAccessors name = genAccessorsWith name (Nothing, []) id

genAccessorsWith :: Name -> CustomConfig -> Renamer -> DecsQ
genAccessorsWith name custom renamer = do
  offsets <- genFieldOffsets name custom
  case offsets of
    (Single s) -> forConstructor s
    (Multiple _ cs) -> do
      ps <- concatMapM forConstructor cs
      let n = mkName ("a'" ++ nameBase name)
      func <- case getDefault custom of
        Just f -> do
          (t, c, _, _) <- f (return (LitE (IntegerL 0)))
          return [SigD n t, FunD n [Clause [] (NormalB c) []]]
        Nothing ->
          return
            [ SigD n ((ConT ''PtrAccessor `AppT` ConT name) `AppT` ConT ''Int),
              FunD n [Clause [] (NormalB (ConE 'PtrAccessor `AppE` InfixE Nothing (VarE 'plusPtr) (Just (LitE (IntegerL 0))))) []]
            ]
      return $ func ++ ps
  where
    forConstructor (ct, _, fs, _) =
      concatMapM
        ( \(offset, fn, _, ft) ->
            let n = mkName ("a'" ++ nameBase name ++ "'" ++ renamer fn)
             in case get fn custom of
                  Just f -> do
                    (t, c, _, _) <- f (return offset)
                    return [SigD n t, FunD n [Clause [] (NormalB c) []]]
                  Nothing -> case ft of
                    Plain t ->
                      return
                        [ SigD n ((ConT ''PtrAccessor `AppT` ct) `AppT` t),
                          FunD n [Clause [] (NormalB (ConE 'PtrAccessor `AppE` InfixE Nothing (VarE 'plusPtr) (Just offset))) []]
                        ]
                    Array t ->
                      return
                        [ SigD n (((ConT ''ArrayAccessor `AppT` ct) `AppT` t) `AppT` ConT ''Int),
                          FunD n [Clause [] (NormalB ((VarE 'arrayAccessor `AppE` InfixE Nothing (VarE 'plusPtr) (Just (InfixE (Just offset) (VarE '(+)) (Just (LitE (IntegerL 8)))))) `AppE` InfixE Nothing (VarE 'plusPtr) (Just offset))) []]
                        ]
        )
        fs

genStorable :: Name -> DecsQ
genStorable name = genStorableWith name (Nothing, []) id

genStorableWith :: Name -> CustomConfig -> Renamer -> DecsQ
genStorableWith name custom renamer = do
  offsets <- genFieldOffsets name custom
  case offsets of
    (Single (t, n, fs, size)) ->
      return
        [ InstanceD
            Nothing
            []
            (ConT ''Storable `AppT` t)
            [ FunD 'sizeOf [Clause [WildP] (NormalB size) []],
              FunD 'alignment [Clause [WildP] (NormalB (VarE 'highestPowerOfTwo `AppE` size)) []],
              let p = mkName "_p"
               in FunD
                    'peek
                    [ Clause
                        [VarP p]
                        ( NormalB
                            ( DoE
                                Nothing
                                (peekFields p n fs)
                            )
                        )
                        []
                    ],
              let p = mkName "_p"
                  pat = ConP n [] (map (\(_, fn, _, _) -> VarP (mkName ('_' : fn))) fs)
               in FunD
                    'poke
                    [ Clause
                        [VarP p, pat]
                        ( NormalB
                            ( DoE
                                Nothing
                                (pokeFields p fs)
                            )
                        )
                        []
                    ]
            ]
        ]
    (Multiple _ xs@((t, _, _, _) : _)) ->
      let msize = VarE 'maximum `AppE` ListE (map (\(_, _, _, size) -> size) xs)
       in return
            [ InstanceD
                Nothing
                []
                (ConT ''Storable `AppT` t)
                [ FunD 'sizeOf [Clause [WildP] (NormalB msize) []],
                  FunD 'alignment [Clause [WildP] (NormalB (VarE 'highestPowerOfTwo `AppE` msize)) []],
                  let p = mkName "_p"
                   in FunD
                        'peek
                        [ Clause
                            [VarP p]
                            ( NormalB
                                ( DoE
                                    Nothing
                                    ( let tn = mkName "_t"
                                       in [ BindS (VarP tn) (VarE 'readAccessor `AppE` VarE (mkName ("a'" ++ nameBase name)) `AppE` VarE p),
                                            NoBindS (CaseE (VarE tn) (zipWith (\i (_, n, fs, _) -> Match (LitP (IntegerL i)) (NormalB (DoE Nothing (peekFields p n fs))) []) [0 ..] xs))
                                          ]
                                    )
                                )
                            )
                            []
                        ],
                  let p = mkName "_p"
                   in FunD
                        'poke
                        ( zipWith
                            ( \i (_, n, fs, _) ->
                                let pat = ConP n [] (map (\(_, fn, _, _) -> VarP (mkName ('_' : fn))) fs)
                                 in Clause
                                      [VarP p, pat]
                                      ( NormalB
                                          ( DoE
                                              Nothing
                                              (NoBindS (VarE 'writeAccessor `AppE` VarE (mkName ("a'" ++ nameBase name)) `AppE` VarE p `AppE` LitE (IntegerL i)) : pokeFields p fs)
                                          )
                                      )
                                      []
                            )
                            [0 ..]
                            xs
                        )
                ]
            ]
    (Multiple _ []) -> fail "must have at least one constructor!"
  where
    peekFields p cn fs =
      map
        ( \(_, fn, _, ft) ->
            BindS
              (VarP (mkName ('_' : fn)))
              ( case ft of
                  Plain _ -> VarE 'readAccessor `AppE` VarE (mkName ("a'" ++ nameBase name ++ "'" ++ renamer fn)) `AppE` VarE p
                  Array _ -> VarE 'readArray `AppE` VarE (mkName ("a'" ++ nameBase name ++ "'" ++ renamer fn)) `AppE` VarE p
              )
        )
        fs
        ++ [NoBindS (VarE 'return `AppE` foldl (\acc (_, fn, _, _) -> acc `AppE` VarE (mkName ('_' : fn))) (ConE cn) fs)]
    pokeFields p =
      map
        ( \(_, fn, _, ft) ->
            NoBindS
              ( case ft of
                  Plain _ -> VarE 'writeAccessor `AppE` VarE (mkName ("a'" ++ nameBase name ++ "'" ++ renamer fn)) `AppE` VarE p `AppE` VarE (mkName ('_' : fn))
                  Array _ -> VarE 'writeArray `AppE` VarE (mkName ("a'" ++ nameBase name ++ "'" ++ renamer fn)) `AppE` VarE p `AppE` VarE (mkName ('_' : fn))
              )
        )

genFreeable :: Name -> DecsQ
genFreeable name = genFreeableWith name (Nothing, []) id

genFreeableWith :: Name -> CustomConfig -> Renamer -> DecsQ
genFreeableWith name custom renamer = do
  offsets <- genFieldOffsets name custom
  case offsets of
    (Single (t, _, fs, _)) ->
      return
        [ InstanceD
            Nothing
            []
            (ConT ''Freeable `AppT` t)
            [ let p = mkName "_p"
               in FunD
                    'freeDependents
                    [ Clause
                        [VarP p]
                        ( NormalB
                            ( DoE
                                Nothing
                                (freeFields p fs)
                            )
                        )
                        []
                    ]
            ]
        ]
    (Multiple _ xs@((t, _, _, _) : _)) ->
      return
        [ InstanceD
            Nothing
            []
            (ConT ''Freeable `AppT` t)
            [ let p = mkName "_p"
               in FunD
                    'freeDependents
                    [ Clause
                        [VarP p]
                        ( NormalB
                            ( DoE
                                Nothing
                                ( let tn = mkName "_t"
                                   in [ BindS (VarP tn) (VarE 'readAccessor `AppE` VarE (mkName ("a'" ++ nameBase name)) `AppE` VarE p),
                                        NoBindS (CaseE (VarE tn) (zipWith (\i (_, _, fs, _) -> Match (LitP (IntegerL i)) (NormalB (DoE Nothing (freeFields p fs))) []) [0 ..] xs))
                                      ]
                                )
                            )
                        )
                        []
                    ]
            ]
        ]
    (Multiple _ []) -> fail "must have at least one constructor!"
  where
    freeFields p =
      map
        ( \(_, fn, _, ft) ->
            NoBindS
              ( case ft of
                  Plain _ -> VarE 'freeAccessor `AppE` VarE (mkName ("a'" ++ nameBase name ++ "'" ++ renamer fn)) `AppE` VarE p
                  Array _ -> VarE 'freeArray `AppE` VarE (mkName ("a'" ++ nameBase name ++ "'" ++ renamer fn)) `AppE` VarE p
              )
        )

genAll :: Name -> DecsQ
genAll name = genAllWith name (Nothing, []) id

genAllWith :: Name -> CustomConfig -> Renamer -> DecsQ
genAllWith name custom renamer = do
  accessors <- genAccessorsWith name custom renamer
  storable <- genStorableWith name custom renamer
  freeable <- genFreeableWith name custom renamer
  return $ accessors ++ storable ++ freeable

genFieldOffsets :: Name -> CustomConfig -> Q FieldOffsets
genFieldOffsets name custom = do
  info <- reify name
  case info of
    (TyConI (DataD _ _ _ _ cs _)) -> case cs of
      [] -> fail (nameBase name ++ " must have at least one constructor")
      xs -> helper (map getFields xs)
    _ -> fail (nameBase name ++ " must be a `data` type constructor")
  where
    helper :: [ConFields] -> Q FieldOffsets
    helper [(a, b, fs)] = do
      (fs', size) <- helper' fs 0
      return $ Single (a, b, fs', size)
    helper cs = do
      let offset = rsizeOf (0 :: Int)
      cs' <-
        forM
          cs
          ( \(a, b, fs) -> do
              (fs', size) <- helper' fs (fromIntegral offset)
              return (a, b, fs', size)
          )
      return $ Multiple offset cs'

    helper' :: [(String, Type)] -> Integer -> Q ([(Exp, String, Type, AccessorType)], Exp)
    helper' fs originalOffset =
      foldlM
        ( \(prev, offset) (fn, ft) ->
            case get fn custom of
              Just f -> do
                (_, _, s, a) <- f (return offset)
                return (prev ++ [(offset, fn, ft, a)], InfixE (Just offset) (VarE '(+)) (Just s))
              Nothing ->
                accessorType ft >>= \case
                  Array ft' -> do
                    inst <- isInstance ''RStorable [ft']
                    let sizeExp = if inst then LitE (IntegerL 16) else error (show (ppr ft') ++ " is not an instance of `RStorable`!")
                    return (prev ++ [(offset, fn, ft, Array ft')], InfixE (Just offset) (VarE '(+)) (Just sizeExp))
                  Plain ft' -> do
                    inst <- isInstance ''RStorable [ft']
                    let sizeExp = if inst then VarE 'rsizeOf `AppE` SigE (VarE 'undefined) ft' else error (show (ppr ft') ++ " is not an instance of `RStorable`!")
                    return (prev ++ [(offset, fn, ft, Plain ft')], InfixE (Just offset) (VarE '(+)) (Just sizeExp))
        )
        ([], LitE (IntegerL originalOffset))
        fs

    getFields :: Con -> ConFields
    getFields x =
      case x of
        (NormalC n fs) -> (ConT name, n, zip (map (('f' :) . show) [(0 :: Int) ..]) (map snd fs))
        (RecC n fs) -> (ConT name, n, map (\(n1, _, t) -> (nameBase n1, t)) fs)
        (GadtC (n : _) fs t) -> (t, n, zip (map (('f' :) . show) [(0 :: Int) ..]) (map snd fs))
        (RecGadtC (n : _) fs t) -> (t, n, map (\(n1, _, t1) -> (nameBase n1, t1)) fs)
        (InfixC (_, t0) n (_, t1)) -> (ConT name, n, [("f0", t0), ("f1", t1)])
        _ -> error "forall types are not currently supported"

    accessorType :: Type -> Q AccessorType
    accessorType fieldType =
      case fieldType of
        t@(ConT n) -> do
          i <- reify n
          case i of
            (TyConI (TySynD _ _ t')) -> accessorType t'
            _ -> return (Plain t)
        ListT `AppT` t -> return (Array t)
        t -> return (Plain t)

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f v = concat <$> mapM f v